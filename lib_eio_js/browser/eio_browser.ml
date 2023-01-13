(*
 * Copyright (C) 2021-2022 Thomas Leonard
 * Copyright (C) 2022      Patrick Ferris
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

open Brr

module Fiber_context = Eio.Private.Fiber_context
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

module Ctf = Eio.Private.Ctf

module Suspended = struct
  type 'a t = {
    fiber : Eio.Private.Fiber_context.t;
    k : ('a, unit) Effect.Deep.continuation;
  }

  let tid t = Eio.Private.Fiber_context.tid t.fiber

  let continue t v =
    Ctf.note_switch (tid t);
    Effect.Deep.continue t.k v

  let discontinue t ex =
    Ctf.note_switch (tid t);
    Effect.Deep.discontinue t.k ex
end

module Scheduler = struct
  type t = {
    listener : Brr.Ev.listener;
    schedule : (unit -> unit);
  }

  let v ~schedule t =
    let open Brr_io in
    let scheduler = El.div [] in
    let el = El.as_target scheduler in
    let listener = Brr.Ev.listen Message.Ev.message (fun _ev -> schedule t) el in
    let schedule =
      let args = [| Ev.create Message.Ev.message |> Ev.to_jv |] in
      let sched = El.to_jv scheduler in
      fun () -> Jv.call sched "dispatchEvent" args |> ignore
    in
    { listener; schedule }

  let stop t =
    Brr.Ev.unlisten t.listener

  let wakeup t = t.schedule ()
end

type t = {
  (* Suspended fibers waiting to run again.
     [Run_queue] is like [Stdlib.Queue], but allows pushing items
     to the head too, which we need. *)
  mutable run_q : (unit -> unit) Run_queue.t;
  mutable pending_io : int;
  mutable scheduler : Scheduler.t option;
}

let enqueue_thread t k v =
  Run_queue.push t.run_q (fun () -> Suspended.continue k v);
  t.pending_io <- t.pending_io - 1;
  Option.iter Scheduler.wakeup t.scheduler

let enqueue_failed_thread t k v =
  Run_queue.push t.run_q (fun () -> Suspended.discontinue k v);
  t.pending_io <- t.pending_io - 1;
  Option.iter Scheduler.wakeup t.scheduler


type _ Effect.t += Enter_unchecked : (t -> 'a Suspended.t -> unit) -> 'a Effect.t
let enter_unchecked fn = Effect.perform (Enter_unchecked fn)

let enter_io fn =
  enter_unchecked @@ fun st k ->
  st.pending_io <- st.pending_io + 1;
  fn st k

(* Resume the next runnable fiber, if any. *)
let schedule t : unit =
  match Run_queue.pop t.run_q with
  | Some f -> f ();
  | None ->
    if t.pending_io = 0 then begin
      Option.iter Scheduler.stop t.scheduler
    end

module Timeout = struct
  let sleep ~ms =
    enter_io @@ fun st k ->
    let timer = ref None in
    Fiber_context.set_cancel_fn k.fiber (fun exn -> Option.iter G.stop_timer !timer; enqueue_failed_thread st k exn);
    let id = G.set_timeout ~ms (fun () ->
        Fiber_context.clear_cancel_fn k.fiber;
        enqueue_thread st k ()
      ) in
    timer := Some id
end

let await fut =
  enter_io @@ fun st k ->
  let cancelled = ref true in
  Fiber_context.set_cancel_fn k.fiber (fun exn -> cancelled := true; enqueue_failed_thread st k exn);
  Fut.await fut (fun v ->
      Fiber_context.clear_cancel_fn k.fiber;
      if not !cancelled then enqueue_thread st k v
    )

let next_event : 'a Brr.Ev.type' -> Brr.Ev.target -> 'a Brr.Ev.t = fun typ target ->
  let opts = Brr.Ev.listen_opts ~once:true () in
  let listen fn = Brr.Ev.listen ~opts typ fn target in
  enter_io @@ fun st k ->
  let listener = ref None in
  Fiber_context.set_cancel_fn k.fiber (fun exn -> Option.iter Ev.unlisten !listener; enqueue_failed_thread st k exn);
  let v = listen (fun v -> enqueue_thread st k v) in
  listener := Some v

(* Largely based on the Eio_mock.Backend event loop. *)
let run main =
  let run_q = Run_queue.create () in
  let t = { run_q; pending_io = 0; scheduler = None } in
  let scheduler = Scheduler.v ~schedule t in
  t.scheduler <- Some scheduler;
  let rec fork ~new_fiber:fiber fn =
    Effect.Deep.match_with fn ()
      { retc = (fun () -> Fiber_context.destroy fiber; schedule t);
        exnc = (fun ex ->
            let bt = Printexc.get_raw_backtrace () in
            Fiber_context.destroy fiber;
            Printexc.raise_with_backtrace ex bt
          );
        effc = fun (type a) (e : a Effect.t) : ((a, unit) Effect.Deep.continuation -> unit) option ->
          match e with
          | Eio.Private.Effects.Suspend f -> Some (fun k ->
              f fiber (function
                  | Ok v -> Run_queue.push t.run_q (fun () -> Effect.Deep.continue k v)
                  | Error ex -> Run_queue.push t.run_q (fun () -> Effect.Deep.discontinue k ex)
                );
              schedule t
            )
          | Enter_unchecked fn -> Some (fun k ->
              fn t { Suspended.k; fiber };
              schedule t
            )
          | Eio.Private.Effects.Fork (new_fiber, f) -> Some (fun k ->
              Run_queue.push_head t.run_q (Effect.Deep.continue k);
              fork ~new_fiber f
            )
          | Eio.Private.Effects.Get_context -> Some (fun k ->
              Effect.Deep.continue k fiber
            )
          | _ -> None
      }
  in
  let new_fiber = Fiber_context.make_root () in
  let result, r = Fut.create () in
  let () = fork ~new_fiber (fun () -> r (main ())) in
  result
