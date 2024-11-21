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
module Rcfd = Eio_unix.Private.Rcfd
module Poll = Iomux.Poll

type exit = [`Exit_scheduler]

(* The type of items in the run queue. *)
type runnable =
  | IO : runnable                                       (* Reminder to check for IO *)
  | Thread : 'a Suspended.t * 'a -> runnable            (* Resume a fiber with a result value *)
  | Failed_thread : 'a Suspended.t * exn -> runnable    (* Resume a fiber with an exception *)

(* For each FD we track which fibers are waiting for it to become readable/writeable. *)
type fd_event_waiters = {
  read : unit Suspended.t Lwt_dllist.t;
  write : unit Suspended.t Lwt_dllist.t;
}

type t = {
  (* The queue of runnable fibers ready to be resumed. Note: other domains can also add work items here. *)
  run_q : runnable Lf_queue.t;

  poll : Poll.t;
  mutable poll_maxi : int;      (* The highest index ever used in [poll]. *)
  fd_map : (Unix.file_descr, fd_event_waiters) Hashtbl.t;

  (* When adding to [run_q] from another domain, this domain may be sleeping and so won't see the event.
     In that case, [need_wakeup = true] and you must signal using [eventfd]. *)
  eventfd : Rcfd.t;                     (* For sending events. *)
  eventfd_r : Unix.file_descr;          (* For reading events. *)

  mutable active_ops : int;             (* Exit when this is zero and [run_q] and [sleep_q] are empty. *)

  (* If [false], the main thread will check [run_q] before sleeping again
     (possibly because an event has been or will be sent to [eventfd]).
     It can therefore be set to [false] in either of these cases:
     - By the receiving thread because it will check [run_q] before sleeping, or
     - By the sending thread because it will signal the main thread later *)
  need_wakeup : bool Atomic.t;

  sleep_q: Zzz.t;                       (* Fibers waiting for timers. *)

  thread_pool : Eio_unix.Private.Thread_pool.t;
}

(* The message to send to [eventfd] (any character would do). *)
let wake_buffer = Bytes.of_string "!"

(* This can be called from any systhread (including ones not running Eio),
   and also from signal handlers or GC finalizers. It must not take any locks. *)
let wakeup t =
  Atomic.set t.need_wakeup false; (* [t] will check [run_q] after getting the event below *)
  Rcfd.use t.eventfd
    ~if_closed:ignore       (* Domain has shut down (presumably after handling the event) *)
    (fun fd ->
       try
         ignore (Unix.single_write fd wake_buffer 0 1 : int)
       with
       | Unix.Unix_error ((Unix.EAGAIN | EWOULDBLOCK), _, _) ->
         (* If the pipe is full then a wake up is pending anyway. *)
         ()
       | Unix.Unix_error (Unix.EPIPE, _, _) ->
         (* We're shutting down; the event has already been processed. *)
         ()
    )

(* Safe to call from anywhere (other systhreads, domains, signal handlers, GC finalizers) *)
let enqueue_thread t k x =
  Lf_queue.push t.run_q (Thread (k, x));
  if Atomic.get t.need_wakeup then wakeup t

(* Safe to call from anywhere (other systhreads, domains, signal handlers, GC finalizers) *)
let enqueue_failed_thread t k ex =
  Lf_queue.push t.run_q (Failed_thread (k, ex));
  if Atomic.get t.need_wakeup then wakeup t

(* Can only be called from our own domain, so no need to check for wakeup. *)
let enqueue_at_head t k =
  Lf_queue.push_head t.run_q (Thread (k, ()))

let get_waiters t fd =
  match Hashtbl.find_opt t.fd_map fd with
  | Some x -> x
  | None ->
    let x = { read = Lwt_dllist.create (); write = Lwt_dllist.create () } in
    Hashtbl.add t.fd_map fd x;
    x

(* The OS told us that the event pipe is ready. Remove events. *)
let clear_event_fd t =
  let buf = Bytes.create 8 in   (* Read up to 8 events at a time *)
  let got = Unix.read t.eventfd_r buf 0 (Bytes.length buf) in
  assert (got > 0)

(* Update [t.poll]'s entry for [fd] to match [waiters]. *)
let update t waiters fd =
  let fdi = Iomux.Util.fd_of_unix fd in
  let flags =
    match not (Lwt_dllist.is_empty waiters.read),
          not (Lwt_dllist.is_empty waiters.write) with
    | false, false -> Poll.Flags.empty
    | true, false -> Poll.Flags.pollin
    | false, true -> Poll.Flags.pollout
    | true, true -> Poll.Flags.(pollin + pollout)
  in
  if flags = Poll.Flags.empty then (
    Poll.invalidate_index t.poll fdi;
    (* Try to find the new maxi, go back on index until we find the next
       used slot, -1 means none in use. *)
    let rec lower_maxi = function
      | -1 -> t.poll_maxi <- -1
      | index ->
        if Poll.((get_fd t.poll index) <> invalid_fd) then
          t.poll_maxi <- index
        else
          lower_maxi (pred index)
    in
    if fdi = t.poll_maxi then
      lower_maxi (pred fdi);
    Hashtbl.remove t.fd_map fd
  ) else (
    Poll.set_index t.poll fdi fd flags;
    if fdi > t.poll_maxi then
      t.poll_maxi <- fdi
  )

let resume t node =
  t.active_ops <- t.active_ops - 1;
  let k : unit Suspended.t = Lwt_dllist.get node in
  Fiber_context.clear_cancel_fn k.fiber;
  enqueue_thread t k ()

(* Called when poll indicates that an event we requested for [fd] is ready. *)
let ready t _index fd revents =
  assert (not Poll.Flags.(mem revents pollnval));
  if fd == t.eventfd_r then (
    clear_event_fd t
    (* The scheduler will now look at the run queue again and notice any new items. *)
  ) else (
    let waiters = Hashtbl.find t.fd_map fd in
    let pending = Lwt_dllist.create () in
    if Poll.Flags.(mem revents (pollout + pollhup + pollerr)) then
      Lwt_dllist.transfer_l waiters.write pending;
    if Poll.Flags.(mem revents (pollin + pollhup + pollerr)) then
      Lwt_dllist.transfer_l waiters.read pending;
    (* If pending has things, it means we modified the waiters, refresh our view *)
    if not (Lwt_dllist.is_empty pending) then
      update t waiters fd;
    Lwt_dllist.iter_node_r (resume t) pending
  )

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
      let timeout =
        match next_due with
        | `Wait_until time ->
          let time = Mtime.to_uint64_ns time in
          let now = Mtime.to_uint64_ns now in
          let diff_ns = Int64.sub time now in
          Poll.Nanoseconds diff_ns
        | `Nothing -> Poll.Infinite
      in
      if timeout = Infinite && t.active_ops = 0 && Lf_queue.is_empty t.run_q then (
        (* Nothing further can happen at this point. *)
        Lf_queue.close t.run_q;      (* Just to catch bugs if something tries to enqueue later *)
        `Exit_scheduler
      ) else (
        Atomic.set t.need_wakeup true;
        let timeout =
          if Lf_queue.is_empty t.run_q then timeout
          else (
            (* Either we're just checking for IO to avoid starvation, or
               someone added a new job while we were setting [need_wakeup] to [true].
               They might or might not have seen that, so we can't be sure they'll send an event. *)
            Poll.Nowait
          )
        in
        (* At this point we're not going to check [run_q] again before sleeping.
           If [need_wakeup] is still [true], this is fine because we don't promise to do that.
           If [need_wakeup = false], a wake-up event will arrive and wake us up soon. *)
        Trace.suspend_domain Begin;
        let nready =
          try Poll.ppoll_or_poll t.poll (t.poll_maxi + 1) timeout
          with Unix.Unix_error (Unix.EINTR, _, "") -> 0
        in
        Trace.suspend_domain End;
        Atomic.set t.need_wakeup false;
        Lf_queue.push t.run_q IO;                   (* Re-inject IO job in the run queue *)
        Poll.iter_ready t.poll nready (ready t);
        next t
      )

let with_sched fn =
  let run_q = Lf_queue.create () in
  Lf_queue.push run_q IO;
  let sleep_q = Zzz.create () in
  let eventfd_r, eventfd_w = Unix.pipe ~cloexec:true () in
  Unix.set_nonblock eventfd_r;
  Unix.set_nonblock eventfd_w;
  let eventfd = Rcfd.make eventfd_w in
  let cleanup () =
    Unix.close eventfd_r;
    let was_open = Rcfd.close eventfd in
    assert was_open
  in
  let poll = Poll.create () in
  let fd_map = Hashtbl.create 10 in
  let thread_pool = Eio_unix.Private.Thread_pool.create ~sleep_q in
  let t = { run_q; poll; poll_maxi = (-1); fd_map; eventfd; eventfd_r;
            active_ops = 0; need_wakeup = Atomic.make false; sleep_q; thread_pool } in
  let eventfd_ri = Iomux.Util.fd_of_unix eventfd_r in
  Poll.set_index t.poll eventfd_ri eventfd_r Poll.Flags.pollin;
  if eventfd_ri > t.poll_maxi then
    t.poll_maxi <- eventfd_ri;
  match fn t with
  | x -> cleanup (); x
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    cleanup ();
    Printexc.raise_with_backtrace ex bt

let await_readable t (k : unit Suspended.t) fd =
  match Fiber_context.get_error k.fiber with
  | Some e -> Suspended.discontinue k e
  | None ->
    t.active_ops <- t.active_ops + 1;
    let waiters = get_waiters t fd in
    let was_empty = Lwt_dllist.is_empty waiters.read in
    let node = Lwt_dllist.add_l k waiters.read in
    if was_empty then update t waiters fd;
    Fiber_context.set_cancel_fn k.fiber (fun ex ->
        Lwt_dllist.remove node;
        if Lwt_dllist.is_empty waiters.read then
          update t waiters fd;
        t.active_ops <- t.active_ops - 1;
        enqueue_failed_thread t k ex
      );
    next t

let await_writable t (k : unit Suspended.t) fd =
  match Fiber_context.get_error k.fiber with
  | Some e -> Suspended.discontinue k e
  | None ->
    t.active_ops <- t.active_ops + 1;
    let waiters = get_waiters t fd in
    let was_empty = Lwt_dllist.is_empty waiters.write in
    let node = Lwt_dllist.add_l k waiters.write in
    if was_empty then update t waiters fd;
    Fiber_context.set_cancel_fn k.fiber (fun ex ->
        Lwt_dllist.remove node;
        if Lwt_dllist.is_empty waiters.write then
          update t waiters fd;
        t.active_ops <- t.active_ops - 1;
        enqueue_failed_thread t k ex
      );
    next t

let get_enqueue t k = function
  | Ok v -> enqueue_thread t k v
  | Error ex -> enqueue_failed_thread t k ex

let await_timeout t (k : unit Suspended.t) time =
  match Fiber_context.get_error k.fiber with
  | Some e -> Suspended.discontinue k e
  | None ->
    let node = Zzz.add t.sleep_q time (Fiber k) in
    Fiber_context.set_cancel_fn k.fiber (fun ex ->
        Zzz.remove t.sleep_q node;
        enqueue_failed_thread t k ex
      );
    next t

let with_op t fn x =
  t.active_ops <- t.active_ops + 1;
  match fn x with
  | r ->
    t.active_ops <- t.active_ops - 1;
    r
  | exception ex ->
    t.active_ops <- t.active_ops - 1;
    raise ex

[@@@alert "-unstable"]

type _ Effect.t += Enter : (t -> 'a Eio_utils.Suspended.t -> [`Exit_scheduler]) -> 'a Effect.t

let enter op fn =
  Trace.suspend_fiber op;
  Effect.perform (Enter fn)

let run ~extra_effects t main x =
  let rec fork ~new_fiber:fiber fn =
    let open Effect.Deep in
    Trace.fiber (Fiber_context.tid fiber);
    match_with fn ()
      { retc = (fun () -> Fiber_context.destroy fiber; next t);
        exnc = (fun ex ->
            Fiber_context.destroy fiber;
            Printexc.raise_with_backtrace ex (Printexc.get_raw_backtrace ())
          );
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Enter fn -> Some (fun k ->
              match Fiber_context.get_error fiber with
              | Some e -> discontinue k e
              | None -> fn t { Suspended.k; fiber }
            )
          | Eio.Private.Effects.Get_context -> Some (fun k -> continue k fiber)
          | Eio.Private.Effects.Suspend f -> Some (fun k ->
              let k = { Suspended.k; fiber } in
              let enqueue = get_enqueue t k in
              f fiber enqueue;
              next t
            )
          | Eio.Private.Effects.Fork (new_fiber, f) -> Some (fun k ->
              let k = { Suspended.k; fiber } in
              enqueue_at_head t k;
              fork ~new_fiber f
            )
          | Eio_unix.Private.Await_readable fd -> Some (fun k ->
              await_readable t { Suspended.k; fiber } fd
            )
          | Eio_unix.Private.Await_writable fd -> Some (fun k ->
              await_writable t { Suspended.k; fiber } fd
            )
          | Eio_unix.Private.Thread_pool.Run_in_systhread fn -> Some (fun k ->
              let k = { Suspended.k; fiber } in
              let enqueue x = enqueue_thread t k (x, t.thread_pool) in
              Eio_unix.Private.Thread_pool.submit t.thread_pool ~ctx:fiber ~enqueue fn;
              next t
            )
          | e -> extra_effects.Effect.Deep.effc e
      }
  in
  let result = ref None in
  let `Exit_scheduler =
    let new_fiber = Fiber_context.make_root () in
    Domain_local_await.using
      ~prepare_for_await:Eio_utils.Dla.prepare_for_await
      ~while_running:(fun () ->
          fork ~new_fiber (fun () ->
              Eio_unix.Private.Thread_pool.run t.thread_pool @@ fun () ->
              result := Some (with_op t main x);
            )
        )
  in
  match !result with
  | Some x -> x
  | None -> failwith "BUG in scheduler: deadlock detected"
