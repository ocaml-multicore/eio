[@@@alert "-unstable"]

open Eio.Std

module Fiber_context = Eio.Private.Fiber_context
module Ctf = Eio.Private.Ctf

module Suspended = Eio_utils.Suspended
module Zzz = Eio_utils.Zzz
module Lf_queue = Eio_utils.Lf_queue

let system_thread = Ctf.mint_id ()

type exit = [`Exit_scheduler]

type file_offset = [
  | `Pos of Optint.Int63.t
  | `Seekable_current
  | `Nonseekable_current
]

type amount = Exactly of int | Upto of int

type rw_req = {
  op : [`R|`W];
  file_offset : file_offset;    (* Read from here + cur_off (unless using current pos) *)
  fd : Unix.file_descr;
  len : amount;
  buf : Uring.Region.chunk;
  mutable cur_off : int;
  action : int Suspended.t;
}

(* Type of user-data attached to jobs. *)
type io_job =
  | Read : rw_req -> io_job
  | Job_no_cancel : int Suspended.t -> io_job
  | Cancel_job : io_job
  | Job : int Suspended.t -> io_job     (* A negative result indicates error, and may report cancellation *)
  | Write : rw_req -> io_job
  | Job_fn : 'a Suspended.t * (int -> [`Exit_scheduler]) -> io_job
  (* When done, remove the cancel_fn from [Suspended.t] and call the callback (unless cancelled). *)

type runnable =
  | IO : runnable
  | Thread : 'a Suspended.t * 'a -> runnable
  | Failed_thread : 'a Suspended.t * exn -> runnable

type t = {
  uring: io_job Uring.t;
  mem: Uring.Region.t option;
  io_q: (t -> unit) Queue.t;     (* waiting for room on [uring] *)
  mem_q : Uring.Region.chunk Suspended.t Queue.t;

  (* The queue of runnable fibers ready to be resumed. Note: other domains can also add work items here. *)
  run_q : runnable Lf_queue.t;

  (* When adding to [run_q] from another domain, this domain may be sleeping and so won't see the event.
     In that case, [need_wakeup = true] and you must signal using [eventfd]. *)
  eventfd : Fd.t;

  (* If [false], the main thread will check [run_q] before sleeping again
     (possibly because an event has been or will be sent to [eventfd]).
     It can therefore be set to [false] in either of these cases:
     - By the receiving thread because it will check [run_q] before sleeping, or
     - By the sending thread because it will signal the main thread later *)
  need_wakeup : bool Atomic.t;

  sleep_q: Zzz.t;
}

type _ Effect.t +=
  | Enter : (t -> 'a Suspended.t -> unit) -> 'a Effect.t
  | Cancel : io_job Uring.job -> unit Effect.t
  | Alloc : Uring.Region.chunk option Effect.t
  | Alloc_or_wait : Uring.Region.chunk Effect.t
  | Free : Uring.Region.chunk -> unit Effect.t

let wake_buffer =
  let b = Bytes.create 8 in
  Bytes.set_int64_ne b 0 1L;
  b

(* This can be called from any systhread (including ones not running Eio),
   and also from signal handlers or GC finalizers. It must not take any locks. *)
let wakeup t =
  Atomic.set t.need_wakeup false; (* [t] will check [run_q] after getting the event below *)
  Fd.use t.eventfd
    (fun fd ->
       let sent = Unix.single_write fd wake_buffer 0 8 in
       assert (sent = 8)
    )
    ~if_closed:ignore   (* Domain has shut down (presumably after handling the event) *)

(* Safe to call from anywhere (other systhreads, domains, signal handlers, GC finalizers) *)
let enqueue_thread st k x =
  Lf_queue.push st.run_q (Thread (k, x));
  if Atomic.get st.need_wakeup then wakeup st

(* Safe to call from anywhere (other systhreads, domains, signal handlers, GC finalizers) *)
let enqueue_failed_thread st k ex =
  Lf_queue.push st.run_q (Failed_thread (k, ex));
  if Atomic.get st.need_wakeup then wakeup st

(* Can only be called from our own domain, so no need to check for wakeup. *)
let enqueue_at_head st k x =
  Lf_queue.push_head st.run_q (Thread (k, x))

let enter fn = Effect.perform (Enter fn)

let rec enqueue_job t fn =
  match fn () with
  | Some _ as r -> r
  | None ->
    if Uring.submit t.uring > 0 then enqueue_job t fn
    else None

(* Cancellations always come from the same domain, so no need to send wake events here. *)
let rec enqueue_cancel job t =
  Ctf.label "cancel";
  match enqueue_job t (fun () -> Uring.cancel t.uring job Cancel_job) with
  | None -> Queue.push (fun t -> enqueue_cancel job t) t.io_q
  | Some _ -> ()

let cancel job = Effect.perform (Cancel job)

(* Cancellation

   For operations that can be cancelled we need to set the fiber's cancellation function.
   The typical sequence is:

   1. We submit an operation, getting back a uring job (needed for cancellation).
   2. We set the cancellation function. The function uses the uring job to cancel.

   When the job completes, we clear the cancellation function. The function
   must have been set by this point because we don't poll for completions until
   the above steps have finished.

   If the context is cancelled while the operation is running, the function will get removed and called,
   which will submit a cancellation request to uring. We know the job is still valid at this point because
   we clear the cancel function when it completes.

   If the operation completes before Linux processes the cancellation, we get [ENOENT], which we ignore. *)

(* [with_cancel_hook ~action t fn] calls [fn] to create a job,
   then sets the fiber's cancel function to cancel it.
   If [action] is already cancelled, it schedules [action] to be discontinued.
   @return Whether to retry the operation later, once there is space. *)
let with_cancel_hook ~action t fn =
  match Fiber_context.get_error action.Suspended.fiber with
  | Some ex -> enqueue_failed_thread t action ex; false
  | None ->
    match enqueue_job t fn with
    | None -> true
    | Some job ->
      Fiber_context.set_cancel_fn action.fiber (fun _ -> cancel job);
      false

let submit_pending_io st =
  match Queue.take_opt st.io_q with
  | None -> ()
  | Some fn ->
    Ctf.label "submit_pending_io";
    fn st

let rec submit_rw_req st ({op; file_offset; fd; buf; len; cur_off; action} as req) =
  let {uring;io_q;_} = st in
  let off = Uring.Region.to_offset buf + cur_off in
  let len = match len with Exactly l | Upto l -> l in
  let len = len - cur_off in
  let retry = with_cancel_hook ~action st (fun () ->
      let file_offset =
        match file_offset with
        | `Pos x -> Optint.Int63.add x (Optint.Int63.of_int cur_off)
        | `Seekable_current -> Optint.Int63.minus_one
        | `Nonseekable_current -> Optint.Int63.zero
      in
      match op with
      |`R -> Uring.read_fixed uring ~file_offset fd ~off ~len (Read req)
      |`W -> Uring.write_fixed uring ~file_offset fd ~off ~len (Write req)
    )
  in
  if retry then (
    Ctf.label "await-sqe";
    (* wait until an sqe is available *)
    Queue.push (fun st -> submit_rw_req st req) io_q
  )

(* TODO bind from unixsupport *)
let errno_is_retry = function -62 | -11 | -4 -> true |_ -> false

(* Switch control to the next ready continuation.
   If none is ready, wait until we get an event to wake one and then switch.
   Returns only if there is nothing to do and no queued operations. *)
let rec schedule ({run_q; sleep_q; mem_q; uring; _} as st) : [`Exit_scheduler] =
  (* This is not a fair scheduler *)
  (* Wakeup any paused fibers *)
  match Lf_queue.pop run_q with
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
    match Zzz.pop ~now sleep_q with
    | `Due k ->
      Lf_queue.push run_q IO;                   (* Re-inject IO job in the run queue *)
      Suspended.continue k ()                   (* A sleeping task is now due *)
    | `Wait_until _ | `Nothing as next_due ->
      (* Handle any pending events before submitting. This is faster. *)
      match Uring.get_cqe_nonblocking uring with
      | Some { data = runnable; result } ->
        Lf_queue.push run_q IO;                   (* Re-inject IO job in the run queue *)
        handle_complete st ~runnable result
      | None ->
        ignore (Uring.submit uring : int);
        let timeout =
          match next_due with
          | `Wait_until time ->
            let time = Mtime.to_uint64_ns time in
            let now = Mtime.to_uint64_ns now in
            let diff_ns = Int64.sub time now |> Int64.to_float in
            Some (diff_ns /. 1e9)
          | `Nothing -> None
        in
        if not (Lf_queue.is_empty st.run_q) then (
          Lf_queue.push run_q IO;                   (* Re-inject IO job in the run queue *)
          schedule st
        ) else if timeout = None && Uring.active_ops uring = 0 then (
          (* Nothing further can happen at this point.
             If there are no events in progress but also still no memory available, something has gone wrong! *)
          assert (Queue.length mem_q = 0);
          Lf_queue.close st.run_q;      (* Just to catch bugs if something tries to enqueue later *)
          `Exit_scheduler
        ) else (
          Atomic.set st.need_wakeup true;
          if Lf_queue.is_empty st.run_q then (
            (* At this point we're not going to check [run_q] again before sleeping.
               If [need_wakeup] is still [true], this is fine because we don't promise to do that.
               If [need_wakeup = false], a wake-up event will arrive and wake us up soon. *)
            Ctf.(note_hiatus Wait_for_work);
            let result = Uring.wait ?timeout uring in
            Ctf.note_resume system_thread;
            Atomic.set st.need_wakeup false;
            Lf_queue.push run_q IO;                   (* Re-inject IO job in the run queue *)
            match result with
            | None ->
              (* Woken by a timeout, which is now due, or by a signal. *)
              schedule st
            | Some { data = runnable; result } ->
              handle_complete st ~runnable result
          ) else (
            (* Someone added a new job while we were setting [need_wakeup] to [true].
               They might or might not have seen that, so we can't be sure they'll send an event. *)
            Atomic.set st.need_wakeup false;
            Lf_queue.push run_q IO;                   (* Re-inject IO job in the run queue *)
            schedule st
          )
        )
and handle_complete st ~runnable result =
  submit_pending_io st;                       (* If something was waiting for a slot, submit it now. *)
  match runnable with
  | Read req ->
    complete_rw_req st req result
  | Write req ->
    complete_rw_req st req result
  | Job k ->
    Fiber_context.clear_cancel_fn k.fiber;
    if result >= 0 then Suspended.continue k result
    else (
      match Fiber_context.get_error k.fiber with
        | None -> Suspended.continue k result
        | Some e ->
          (* If cancelled, report that instead. *)
          Suspended.discontinue k e
    )
  | Job_no_cancel k ->
    Suspended.continue k result
  | Cancel_job ->
    begin match result with
      | 0     (* Operation cancelled successfully *)
      | -2    (* ENOENT - operation completed before cancel took effect *)
      | -114  (* EALREADY - operation already in progress *)
        -> ()
      | errno ->
        Log.warn (fun f -> f "Cancel returned unexpected error: %s" (Unix.error_message (Uring.error_of_errno errno)))
    end;
    schedule st
  | Job_fn (k, f) ->
    Fiber_context.clear_cancel_fn k.fiber;
    (* Should we only do this on error, to avoid losing the return value?
       We already do that with rw jobs. *)
    begin match Fiber_context.get_error k.fiber with
      | None -> f result
      | Some e -> Suspended.discontinue k e
    end
and complete_rw_req st ({len; cur_off; action; _} as req) res =
  Fiber_context.clear_cancel_fn action.fiber;
  match res, len with
  | 0, _ -> Suspended.discontinue action End_of_file
  | e, _ when e < 0 ->
    begin match Fiber_context.get_error action.fiber with
      | Some e -> Suspended.discontinue action e        (* If cancelled, report that instead. *)
      | None ->
        if errno_is_retry e then (
          submit_rw_req st req;
          schedule st
        ) else (
          Suspended.continue action e
        )
    end
  | n, Exactly len when n < len - cur_off ->
    req.cur_off <- req.cur_off + n;
    submit_rw_req st req;
    schedule st
  | _, Exactly len -> Suspended.continue action len
  | n, Upto _ -> Suspended.continue action n

let alloc_buf_or_wait st k =
  match st.mem with
  | None -> Suspended.discontinue k (Failure "No fixed buffer available")
  | Some mem ->
    match Uring.Region.alloc mem with
    | buf -> Suspended.continue k buf
    | exception Uring.Region.No_space ->
      Queue.push k st.mem_q;
      schedule st

let free_buf st buf =
  match Queue.take_opt st.mem_q with
  | None -> Uring.Region.free buf
  | Some k -> enqueue_thread st k buf

let rec enqueue_poll_add fd poll_mask st action =
  Ctf.label "poll_add";
  let retry = with_cancel_hook ~action st (fun () ->
      Uring.poll_add st.uring fd poll_mask (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_poll_add fd poll_mask st action) st.io_q

let rec enqueue_poll_add_unix fd poll_mask st action cb =
  Ctf.label "poll_add";
  let retry = with_cancel_hook ~action st (fun () ->
      Uring.poll_add st.uring fd poll_mask (Job_fn (action, cb))
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_poll_add_unix fd poll_mask st action cb) st.io_q

let rec enqueue_readv args st action =
  let (file_offset,fd,bufs) = args in
  Ctf.label "readv";
  let retry = with_cancel_hook ~action st (fun () ->
      Uring.readv st.uring ~file_offset fd bufs (Job action))
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_readv args st action) st.io_q

let read_eventfd fd buf =
  let res = enter (enqueue_readv (Optint.Int63.zero, fd, [buf])) in
  if res < 0 then (
    raise @@ Unix.Unix_error (Uring.error_of_errno res, "readv", "")
  ) else if res = 0 then (
    raise End_of_file
  ) else (
    res
  )

let monitor_event_fd t =
  let buf = Cstruct.create 8 in
  Fd.use_exn "monitor_event_fd" t.eventfd @@ fun fd ->
  while true do
    let got = read_eventfd fd buf in
    assert (got = 8);
    (* We just go back to sleep now, but this will cause the scheduler to look
       at the run queue again and notice any new items. *)
  done;
  assert false

let run ~extra_effects st main arg =
  let rec fork ~new_fiber:fiber fn =
    let open Effect.Deep in
    Ctf.note_switch (Fiber_context.tid fiber);
    match_with fn ()
      { retc = (fun () -> Fiber_context.destroy fiber; schedule st);
        exnc = (fun ex ->
            Fiber_context.destroy fiber;
            Printexc.raise_with_backtrace ex (Printexc.get_raw_backtrace ())
          );
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Enter fn -> Some (fun k ->
              match Fiber_context.get_error fiber with
              | Some e -> discontinue k e
              | None ->
                let k = { Suspended.k; fiber } in
                fn st k;
                schedule st
            )
          | Cancel job -> Some (fun k ->
              enqueue_cancel job st;
              continue k ()
            )
          | Eio.Private.Effects.Get_context -> Some (fun k -> continue k fiber)
          | Eio.Private.Effects.Suspend f -> Some (fun k ->
              let k = { Suspended.k; fiber } in
              f fiber (function
                  | Ok v -> enqueue_thread st k v
                  | Error ex -> enqueue_failed_thread st k ex
                );
              schedule st
            )
          | Eio.Private.Effects.Fork (new_fiber, f) -> Some (fun k ->
              let k = { Suspended.k; fiber } in
              enqueue_at_head st k ();
              fork ~new_fiber f
            )
          | Eio_unix.Private.Await_readable fd -> Some (fun k ->
              match Fiber_context.get_error fiber with
              | Some e -> discontinue k e
              | None ->
                let k = { Suspended.k; fiber } in
                enqueue_poll_add_unix fd Uring.Poll_mask.(pollin + pollerr) st k (fun res ->
                    if res >= 0 then Suspended.continue k ()
                    else Suspended.discontinue k (Unix.Unix_error (Uring.error_of_errno res, "await_readable", ""))
                  );
                schedule st
            )
          | Eio_unix.Private.Await_writable fd -> Some (fun k ->
              match Fiber_context.get_error fiber with
              | Some e -> discontinue k e
              | None ->
                let k = { Suspended.k; fiber } in
                enqueue_poll_add_unix fd Uring.Poll_mask.(pollout + pollerr) st k (fun res ->
                    if res >= 0 then Suspended.continue k ()
                    else Suspended.discontinue k (Unix.Unix_error (Uring.error_of_errno res, "await_writable", ""))
                  );
                schedule st
            )
          | Alloc -> Some (fun k ->
              match st.mem with
              | None -> continue k None
              | Some mem ->
                match Uring.Region.alloc mem with
                | buf -> continue k (Some buf)
                | exception Uring.Region.No_space -> continue k None
            )
          | Alloc_or_wait -> Some (fun k ->
              let k = { Suspended.k; fiber } in
              alloc_buf_or_wait st k
            )
          | Free buf -> Some (fun k ->
              free_buf st buf;
              continue k ()
            )
          | e -> extra_effects.effc e
      }
  in
  let result = ref None in
  let `Exit_scheduler =
    let new_fiber = Fiber_context.make_root () in
    fork ~new_fiber (fun () ->
        Switch.run_protected (fun sw ->
            Switch.on_release sw (fun () ->
                Fd.close st.eventfd
              );
            Fiber.fork_daemon ~sw (fun () -> monitor_event_fd st);
            match main arg with
            | x -> result := Some (Ok x)
            | exception ex ->
              let bt = Printexc.get_raw_backtrace () in
              result := Some (Error (ex, bt))
          )
      )
  in
  match Option.get !result with
  | Ok x -> x
  | Error (ex, bt) -> Printexc.raise_with_backtrace ex bt

type config = {
  queue_depth : int;
  n_blocks : int;
  block_size : int;
  polling_timeout : int option;
}

let config ?(queue_depth=64) ?n_blocks ?(block_size=4096) ?polling_timeout () =
  let n_blocks = Option.value n_blocks ~default:queue_depth in
  {
    queue_depth;
    n_blocks;
    block_size;
    polling_timeout;
  }

external eio_eventfd : int -> Unix.file_descr = "caml_eio_eventfd"

let no_fallback (`Msg msg) = failwith msg

let with_sched ?(fallback=no_fallback) config fn =
  let { queue_depth; n_blocks; block_size; polling_timeout } = config in
  match Uring.create ~queue_depth ?polling_timeout () with
  | exception Unix.Unix_error(Unix.ENOSYS, _, _) -> fallback (`Msg "io_uring is not available on this system")
  | uring ->
    let probe = Uring.get_probe uring in
    if not (Uring.op_supported probe Uring.Op.shutdown) then
      fallback (`Msg "Linux >= 5.11 is required for io_uring support")
    else (
      match
        let mem =
          let fixed_buf_len = block_size * n_blocks in
          let buf = Bigarray.(Array1.create char c_layout fixed_buf_len) in
          match Uring.set_fixed_buffer uring buf with
          | Ok () ->
            Some (Uring.Region.init ~block_size buf n_blocks)
          | Error `ENOMEM ->
            Log.warn (fun f -> f "Failed to allocate %d byte fixed buffer" fixed_buf_len);
            None
        in
        let run_q = Lf_queue.create () in
        Lf_queue.push run_q IO;
        let sleep_q = Zzz.create () in
        let io_q = Queue.create () in
        let mem_q = Queue.create () in
        let eventfd = Fd.of_unix_no_hook ~seekable:false ~close_unix:true (eio_eventfd 0) in
        fn { mem; uring; run_q; io_q; mem_q; eventfd; need_wakeup = Atomic.make false; sleep_q }
      with
      | x -> Uring.exit uring; x
      | exception ex ->
        let bt = Printexc.get_raw_backtrace () in
        begin
          try Uring.exit uring
          with ex2 ->
            let bt2 = Printexc.get_raw_backtrace () in
            raise (Eio.Exn.Multiple [(ex2, bt2); (ex, bt)])
        end;
        Printexc.raise_with_backtrace ex bt
    )
