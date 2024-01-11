(*
 * Copyright (C) 2021-2023 Thomas Leonard
 * Copyright (C) 2023 Patrick Ferris
 * Copyright (C) 2024 Jérôme Vouillon
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

(* The scheduler executes eagerly the active fibers until the run
   queue gets empty. It becomes active when the function [start] is
   called or when a fiber is resumed.

   There is no protection against busy yielding. We expect the
   programs to perform some computations in response to external
   events for some finite amount of time and to be otherwise idle.
*)

module Fiber_context = Eio.Private.Fiber_context
module Trace = Eio.Private.Trace

module Run_queue : sig
  type 'a t
  (* A queue that supports pushing to the head of the queue *)

  val create : unit -> 'a t

  val push : 'a t -> 'a -> unit
  (** [push q v] pushes a new item [v] to the back of the queue [q] *)

  val push_head : 'a t -> 'a -> unit
  (** [push_head q v] pushes a new item [v] to the head of the queue [q] *)

  val pop : 'a t -> 'a option
  (** [pop q] pops the next item of the queue if available. *)
end = struct
  type 'a t = 'a Lwt_dllist.t

  let create () = Lwt_dllist.create ()
  let push q v = ignore (Lwt_dllist.add_r v q : 'a Lwt_dllist.node)
  let push_head q v = ignore (Lwt_dllist.add_l v q : 'a Lwt_dllist.node)
  let pop q = Lwt_dllist.take_opt_l q
end

type suspend = Suspend

module Suspended = struct
  type 'a t = {
    fiber : Eio.Private.Fiber_context.t;
    k : ('a, suspend) Effect.Deep.continuation;
  }

  let tid t = Eio.Private.Fiber_context.tid t.fiber

  let continue t v =
    Trace.fiber (tid t);
    Effect.Deep.continue t.k v

  let discontinue t ex =
    Trace.fiber (tid t);
    Effect.Deep.discontinue t.k ex
end

module Scheduler : sig
  val activate : (unit -> suspend) -> unit
  val next : unit -> suspend
  val enqueue_thread : 'a Suspended.t -> 'a -> unit
  val enqueue_failed_thread : 'a Suspended.t -> exn -> unit
  val enqueue_at_head : 'a Suspended.t -> 'a -> unit
end = struct
  let run_queue = Run_queue.create ()
  let active = ref false

  let resume fn =
    assert (not !active);
    active := true;
    let Suspend = fn () in
    active := false

  let next () =
    assert !active;
    match Run_queue.pop run_queue with Some fn -> fn () | None -> Suspend

  let resume_if_needed () = if not !active then resume next

  let enqueue_thread k v =
    Run_queue.push run_queue (fun () -> Suspended.continue k v);
    resume_if_needed ()

  let enqueue_failed_thread k v =
    Run_queue.push run_queue (fun () -> Suspended.discontinue k v);
    resume_if_needed ()

  let enqueue_at_head k v =
    assert !active;
    Run_queue.push_head run_queue (fun () -> Suspended.continue k v)

  let activate fn = if !active then Run_queue.push run_queue fn else resume fn
end

let default_uncaught_exception_handler exn raw_backtrace =
  Printexc.default_uncaught_exception_handler exn raw_backtrace;
  exit 2

let uncaught_exception_handler = ref default_uncaught_exception_handler
let set_uncaught_exception_handler fn = uncaught_exception_handler := fn

type _ Effect.t += Enter : ('a Suspended.t -> unit) -> 'a Effect.t

let enter fn = Effect.perform (Enter fn)

let start main =
  let rec fork ~new_fiber:fiber fn =
    Effect.Deep.match_with fn ()
      {
        retc =
          (fun () ->
            Fiber_context.destroy fiber;
            Scheduler.next ());
        exnc =
          (fun ex ->
            let bt = Printexc.get_raw_backtrace () in
            Fiber_context.destroy fiber;
            !uncaught_exception_handler ex bt;
            Scheduler.next ());
        effc =
          (fun (type a) (e : a Effect.t) :
               ((a, suspend) Effect.Deep.continuation -> suspend) option ->
            match e with
            | Eio.Private.Effects.Suspend f ->
                Some
                  (fun k ->
                    let k = { Suspended.k; fiber } in
                    f fiber (function
                      | Ok v -> Scheduler.enqueue_thread k v
                      | Error ex -> Scheduler.enqueue_failed_thread k ex);
                    Scheduler.next ())
            | Enter fn ->
                Some
                  (fun k ->
                    match Fiber_context.get_error fiber with
                    | Some exn -> Effect.Deep.discontinue k exn
                    | None ->
                        fn { Suspended.k; fiber };
                        Scheduler.next ())
            | Eio.Private.Effects.Fork (new_fiber, f) ->
                Some
                  (fun k ->
                    let k = { Suspended.k; fiber } in
                    Scheduler.enqueue_at_head k ();
                    fork ~new_fiber f)
            | Eio.Private.Effects.Get_context ->
                Some (fun k -> Effect.Deep.continue k fiber)
            | _ -> None);
      }
  in
  let new_fiber = Fiber_context.make_root () in
  Scheduler.activate (fun () -> fork ~new_fiber main)

let await ~setup ~cancel =
  enter @@ fun k ->
  let resumed = ref false in
  let resolve v =
    if not !resumed then (
      resumed := true;
      Fiber_context.clear_cancel_fn k.fiber;
      Scheduler.enqueue_thread k v)
  in
  let reject exn =
    if not !resumed then (
      resumed := true;
      Fiber_context.clear_cancel_fn k.fiber;
      Scheduler.enqueue_failed_thread k exn)
  in
  Fiber_context.set_cancel_fn k.fiber reject;
  try
    let id = setup ~resolve ~reject in
    Fiber_context.set_cancel_fn k.fiber (fun exn ->
        cancel id;
        reject exn)
  with exn -> reject exn
