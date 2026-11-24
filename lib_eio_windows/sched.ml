(*
 * Copyright (C) 2023 Thomas Leonard
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Suspended = Eio_utils.Suspended
module Zzz = Eio_utils.Zzz
module Lf_queue = Eio_utils.Lf_queue
module Fiber_context = Eio.Private.Fiber_context
module Trace = Eio.Private.Trace

type exit = [`Exit_scheduler]

(* The type of items in the run queue. *)
type runnable =
  | IO : runnable                                       (* Reminder to check for IO *)
  | Thread : 'a Suspended.t * 'a -> runnable            (* Resume a fiber with a result value *)
  | Failed_thread : 'a Suspended.t * exn -> runnable    (* Resume a fiber with an exception *)

type 'a t = {
  (* The queue of runnable fibers ready to be resumed. Note: other domains can also add work items here. *)
  run_q : runnable Lf_queue.t;

  readiness : Readiness.t;
  completion : Completion.t;
  interrupt : Iocp.Interrupt.t;
  sleeping : bool Atomic.t;

  mutable result : 'a option;           (* Set when the root fiber returns. *)

  sleep_q: Zzz.t;                       (* Fibers waiting for timers. *)

  thread_pool : Eio_unix.Private.Thread_pool.t;
}

(* Enqueue may be called from another domain, a signal handler or a finalizer. *)
let enqueue t job =
  Lf_queue.push t.run_q job;
  if Atomic.get t.sleeping && Atomic.exchange t.sleeping false then
    Iocp.Interrupt.signal t.interrupt

let enqueue_result t k = function
  | Ok v -> enqueue t (Thread (k, v))
  | Error ex -> enqueue t (Failed_thread (k, ex))

(* Switch control to the next ready continuation.
   If none is ready, wait until we get an event to wake one and then switch.
   Returns only if there is nothing to do and no active operations. *)
let rec next t : [`Exit_scheduler] =
  (* Wakeup any paused fibers *)
  match Lf_queue.pop t.run_q with
  | None -> assert false    (* We should always have an IO job, at least *)
  | Some Thread (k, v) ->   (* We already have a runnable task *)
    Fiber_context.clear_cancel_fn k.fiber;
    Suspended.continue k v
  | Some Failed_thread (k, ex) ->
    Fiber_context.clear_cancel_fn k.fiber;
    Suspended.discontinue k ex
  | Some IO -> (* Note: be sure to re-inject the IO task before continuing! *)
    (* This is not a fair scheduler: timers always run before all other IO *)
    let now = Mtime_clock.now () in
    match Zzz.pop ~now t.sleep_q with
    | `Due k ->
      (* A sleeping task is now due *)
      Lf_queue.push t.run_q IO;                 (* Re-inject IO job in the run queue *)
      begin match k with
        | Fiber k -> Suspended.continue k ()
        | Fn fn -> fn (); next t
      end
    | `Wait_until _ | `Nothing as next_due ->
      if next_due = `Nothing && Option.is_some t.result && Lf_queue.is_empty t.run_q then (
        Lf_queue.close t.run_q;
        `Exit_scheduler
      ) else (
        (* Publish before checking the queue so a remote enqueue cannot miss us. *)
        Atomic.set t.sleeping true;
        let ms =
          if not (Lf_queue.is_empty t.run_q) then 0
          else match next_due with
            | `Nothing -> -1
            | `Wait_until time ->
              let ns = Mtime.Span.to_float_ns (Mtime.span now time) in
              int_of_float (min (min (float_of_int max_int) 4294967294.) (ceil (ns /. 1e6)))
        in
        Trace.suspend_domain Begin;
        let packet = Iocp.wait t.completion ~timeout:ms in
        Atomic.set t.sleeping false;
        Trace.suspend_domain End;
        Option.iter Completion.dispatch packet;
        Lf_queue.push t.run_q IO;
        next t
      )

let with_sched fn =
  let run_q = Lf_queue.create () in
  Lf_queue.push run_q IO;
  let sleep_q = Zzz.create () in
  let thread_pool = Eio_unix.Private.Thread_pool.create ~sleep_q in
  Completion.with_port @@ fun completion ->
  let interrupt = Iocp.Interrupt.current () in
  Fun.protect ~finally:(fun () -> Iocp.Interrupt.close interrupt) @@ fun () ->
  Readiness.with_worker @@ fun readiness ->
  fn { run_q; sleep_q; thread_pool; readiness; completion; interrupt;
       sleeping = Atomic.make false; result = None }

let await direction t (k : unit Suspended.t) fd =
  match Fiber_context.get_error k.fiber with
  | Some ex -> Suspended.discontinue k ex
  | None ->
    match Readiness.await t.readiness direction fd (enqueue_result t k) with
    | cancel -> Fiber_context.set_cancel_fn k.fiber cancel; next t
    | exception ex -> Suspended.discontinue k ex

let await_timeout t (k : unit Suspended.t) time =
  match Fiber_context.get_error k.fiber with
  | Some e -> Suspended.discontinue k e
  | None ->
    let node = Zzz.add t.sleep_q time (Fiber k) in
    Fiber_context.set_cancel_fn k.fiber (fun ex ->
        Zzz.remove t.sleep_q node;
        enqueue t (Failed_thread (k, ex))
      );
    next t

type _ Effect.t += Sleep_until : Mtime.t -> unit Effect.t
let sleep_until time = Effect.perform (Sleep_until time)

let run ~extra_effects main x =
  with_sched @@ fun t ->
  let rec fork ~new_fiber:fiber fn =
    let open Effect.Deep in
    Trace.fiber (Fiber_context.tid fiber);
    match_with fn ()
      { retc = (fun () -> Fiber_context.destroy fiber; next t);
        exnc = (fun ex ->
            Fiber_context.destroy fiber;
            Printexc.raise_with_backtrace ex (Printexc.get_raw_backtrace ())
          );
        effc = fun (type a) (e : a Effect.t) : ((a, exit) continuation -> exit) option ->
          match e with
          | Completion.Get -> Some (fun k -> continue k t.completion)
          | Sleep_until time -> Some (fun k -> await_timeout t { Suspended.k; fiber } time)
          | Eio.Private.Effects.Get_context -> Some (fun k -> continue k fiber)
          | Eio.Private.Effects.Suspend f -> Some (fun k ->
              let k = { Suspended.k; fiber } in
              f fiber (enqueue_result t k);
              next t
            )
          | Eio.Private.Effects.Fork (new_fiber, f) -> Some (fun k ->
              let k = { Suspended.k; fiber } in
              Lf_queue.push_head t.run_q (Thread (k, ()));
              fork ~new_fiber f
            )
          | Eio_unix.Private.Await_readable fd -> Some (fun k ->
              await `R t { Suspended.k; fiber } fd
            )
          | Eio_unix.Private.Await_writable fd -> Some (fun k ->
              await `W t { Suspended.k; fiber } fd
            )
          | Eio_unix.Private.Thread_pool.Run_in_systhread fn -> Some (fun k ->
              let k = { Suspended.k; fiber } in
              let resume x = enqueue t (Thread (k, (x, t.thread_pool))) in
              Eio_unix.Private.Thread_pool.submit t.thread_pool ~ctx:fiber ~enqueue:resume fn;
              next t
            )
          | e -> extra_effects.Effect.Deep.effc e
      }
  in
  let `Exit_scheduler =
    let new_fiber = Fiber_context.make_root () in
    Domain_local_await.using
      ~prepare_for_await:Eio_utils.Dla.prepare_for_await
      ~while_running:(fun () ->
          fork ~new_fiber (fun () ->
              t.result <- Some (Eio_unix.Private.Thread_pool.run t.thread_pool (fun () -> main x))
            )
        )
  in
  Option.get t.result
