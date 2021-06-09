(*
 * Copyright (C) 2020-2021 Anil Madhavapeddy
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

let src = Logs.Src.create "eunix" ~doc:"Effect-based IO system"
module Log = (val Logs.src_log src : Logs.LOG)

open Eio.Std

(* SIGPIPE makes no sense in a modern application. *)
let () = Sys.(set_signal sigpipe Signal_ignore)

type amount = Exactly of int | Upto of int

let system_thread = Ctf.mint_id ()

effect Close : Unix.file_descr -> int

module FD = struct
  type t = {
    seekable : bool;
    mutable release_hook : unit -> unit;        (* Call this on close to remove switch's [on_release] hook. *)
    mutable fd : [`Open of Unix.file_descr | `Closed]
  }

  let get op = function
    | { fd = `Open fd; _ } -> fd
    | { fd = `Closed ; _ } -> invalid_arg (op ^ ": file descriptor used after calling close!")

  let is_open = function
    | { fd = `Open _; _ } -> true
    | { fd = `Closed; _ } -> false

  let close t =
    Ctf.label "close";
    let fd = get "close" t in
    t.fd <- `Closed;
    t.release_hook ();
    let res = perform (Close fd) in
    Log.debug (fun l -> l "close: woken up");
    if res < 0 then
      raise (Unix.Unix_error (Uring.error_of_errno res, "close", ""))

  let ensure_closed t =
    if is_open t then close t

  let is_seekable fd =
    match Unix.lseek fd 0 Unix.SEEK_CUR with
    | (_ : int) -> true
    | exception Unix.Unix_error(Unix.ESPIPE, "lseek", "") -> false

  let to_unix = get "to_unix"

  let of_unix_no_hook ~seekable fd =
    { seekable; fd = `Open fd; release_hook = ignore }

  let of_unix ~sw ~seekable fd =
    let t = of_unix_no_hook ~seekable fd in
    t.release_hook <- Switch.on_release_cancellable sw (fun () -> close t);
    t

  let uring_file_offset t =
    if t.seekable then Optint.Int63.minus_one else Optint.Int63.zero
end

type rw_req = {
  op : [`R|`W];
  file_offset : Optint.Int63.t;
  fd : FD.t;
  len : amount;
  buf : Uring.Region.chunk;
  mutable cur_off : int;
  action : int Suspended.t;
  sw : Switch.t option;
}

type cancel_hook = Eio.Private.Waiters.waiter ref

(* Type of user-data attached to jobs. *)
type io_job =
  | Read : rw_req * cancel_hook -> io_job
  | Poll_add : int Suspended.t * cancel_hook -> io_job
  | Splice : int Suspended.t * cancel_hook -> io_job
  | Connect : int Suspended.t * cancel_hook -> io_job
  | Accept : int Suspended.t * cancel_hook -> io_job
  | Close : int Suspended.t -> io_job
  | Write : rw_req * cancel_hook -> io_job
  | Cancel : int Suspended.t -> io_job

type runnable =
  | Thread : 'a Suspended.t * 'a -> runnable
  | Failed_thread : 'a Suspended.t * exn -> runnable

type t = {
  uring: io_job Uring.t;
  mem: Uring.Region.t;
  io_q: (t -> unit) Queue.t;     (* waiting for room on [uring] *)
  mem_q : Uring.Region.chunk Suspended.t Queue.t;
  run_q : runnable Queue.t;
  sleep_q: Zzz.t;
  mutable io_jobs: int;
}

let enqueue_thread st k x =
  Queue.push (Thread (k, x)) st.run_q

let enqueue_failed_thread st k ex =
  Queue.push (Failed_thread (k, ex)) st.run_q

effect Cancel : io_job Uring.job -> int
let cancel job =
  let res = perform (Cancel job) in
  if res = 2 then (
    Log.debug (fun f -> f "Cancel returned ENOENT - operation completed before cancel took effect")
  ) else if res <> 0 then (
    raise (Unix.Unix_error (Uring.error_of_errno res, "cancel", ""))
  )

(* Cancellation

   For operations that can be cancelled we need to attach a callback to the
   switch to trigger the cancellation, and we need to remove that callback once
   the operation is complete. The typical sequence is:

   1. We create an io_job with an empty [cancel_hook] (because we haven't registered it yet).
   2. We submit the operation, getting back a uring job (needed for cancellation).
   3. We register a cancellation hook with the switch. The hook uses the uring job to cancel.
   4. We update the [cancel_hook] with the waiter for removing the cancellation hook.
      This is the reason that [cancel_hook] is mutable.

   When the job completes, we get the cancellation hook from the io_job and
   ensure it is removed from the switch, as it's no longer needed. The hook
   must have been set by this point because we don't poll for completions until
   the above steps have all finished.

   If the switch is turned off while the operation is running, the switch will start calling
   the hooks. If it gets to ours before it's removed, we will submit a cancellation request to uring.
   If the operation completes before Linux processes the cancellation, we get [ENOENT], which we ignore.

   If the switch is turned off before starting then we discontinue the fibre. *)

(* [with_cancel_hook ~sw ~action st fn] calls [fn] with a fresh cancel hook.
   When [fn cancel_hook] returns, it registers a cancellation callback with [sw] and stores its handle in [cancel_hook].
   If [sw] is already off, it schedules [action] to be discontinued.
   @return Whether to retry the operation later, once there is space. *)
let with_cancel_hook ?sw ~action st fn =
  let release = ref Eio.Private.Waiters.null in
  match sw with
  | None -> fn release = None
  | Some sw ->
    match Switch.get_error sw with
    | Some ex -> enqueue_failed_thread st action ex; false
    | None ->
      match fn release with
      | None -> true
      | Some job ->
        release := Eio.Private.Switch.add_cancel_hook sw (fun _ -> cancel job);
        false

let rec submit_rw_req st ({op; file_offset; fd; buf; len; cur_off; sw; action} as req) =
  let fd = FD.get "submit_rw_req" fd in
  let {uring;io_q;_} = st in
  let off = Uring.Region.to_offset buf + cur_off in
  let len = match len with Exactly l | Upto l -> l in
  let len = len - cur_off in
  let retry = with_cancel_hook ?sw ~action st (fun cancel ->
      match op with
      |`R -> Uring.read uring ~file_offset fd off len (Read (req, cancel))
      |`W -> Uring.write uring ~file_offset fd off len (Write (req, cancel))
    )
  in
  if retry then (
    Ctf.label "await-sqe";
    (* wait until an sqe is available *)
    Queue.push (fun st -> submit_rw_req st req) io_q
  )

(* TODO bind from unixsupport *)
let errno_is_retry = function -62 | -11 | -4 -> true |_ -> false

let enqueue_read st action (sw,file_offset,fd,buf,len) =
  let file_offset =
    match file_offset with
    | Some x -> x
    | None -> FD.uring_file_offset fd
  in
  let req = { op=`R; file_offset; len; fd; cur_off = 0; buf; action; sw} in
  Log.debug (fun l -> l "read: submitting call");
  Ctf.label "read";
  submit_rw_req st req

let rec enqueue_poll_add ?sw st action fd poll_mask =
  Log.debug (fun l -> l "poll_add: submitting call");
  Ctf.label "poll_add";
  let retry = with_cancel_hook ?sw ~action st (fun cancel ->
      Uring.poll_add st.uring (FD.get "poll_add" fd) poll_mask (Poll_add (action, cancel))
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_poll_add ?sw st action fd poll_mask) st.io_q

let rec enqueue_close st action fd =
  Log.debug (fun l -> l "close: submitting call");
  Ctf.label "close";
  let subm = Uring.close st.uring fd (Close action) in
  if subm = None then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_close st action fd) st.io_q

let enqueue_write st action (sw,file_offset,fd,buf,len) =
  let file_offset =
    match file_offset with
    | Some x -> x
    | None -> FD.uring_file_offset fd
  in
  let req = { op=`W; file_offset; len; fd; cur_off = 0; buf; action; sw } in
  Log.debug (fun l -> l "write: submitting call");
  Ctf.label "write";
  submit_rw_req st req

let rec enqueue_splice ?sw st action ~src ~dst ~len =
  Log.debug (fun l -> l "splice: submitting call");
  Ctf.label "splice";
  let retry = with_cancel_hook ?sw ~action st (fun cancel ->
      Uring.splice st.uring (Splice (action, cancel)) ~src:(FD.get "splice" src) ~dst:(FD.get "splice" dst) ~len
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_splice ?sw st action ~src ~dst ~len) st.io_q

let rec enqueue_connect ?sw st action fd addr =
  Log.debug (fun l -> l "connect: submitting call");
  Ctf.label "connect";
  let retry = with_cancel_hook ?sw ~action st (fun cancel ->
      Uring.connect st.uring (FD.get "connect" fd) addr (Connect (action, cancel))
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_connect ?sw st action fd addr) st.io_q

let rec enqueue_cancel st action job =
  Log.debug (fun l -> l "cancel: submitting call");
  Ctf.label "cancel";
  match Uring.cancel st.uring job (Cancel action) with
  | None -> Queue.push (fun st -> enqueue_cancel st action job) st.io_q
  | Some _ -> ()

let rec enqueue_accept ~sw st action fd client_addr =
  Log.debug (fun l -> l "accept: submitting call");
  Ctf.label "accept";
  let retry = with_cancel_hook ~sw ~action st (fun cancel ->
      Uring.accept st.uring (FD.get "accept" fd) client_addr (Accept (action, cancel))
    ) in
  if retry then (
    (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_accept ~sw st action fd client_addr) st.io_q
  )

let submit_pending_io st =
  match Queue.take_opt st.io_q with
  | None -> ()
  | Some fn ->
    Ctf.label "submit_pending_io";
    fn st

(* Switch control to the next ready continuation.
   If none is ready, wait until we get an event to wake one and then switch.
   Returns only if there is nothing to do and no queued operations. *)
let rec schedule ({run_q; sleep_q; mem_q; uring; _} as st) : [`Exit_scheduler] =
  (* This is not a fair scheduler *)
  (* Wakeup any paused fibres *)
  match Queue.take run_q with
  | Thread (k, v) -> Suspended.continue k v               (* We already have a runnable task *)
  | Failed_thread (k, ex) -> Suspended.discontinue k ex
  | exception Queue.Empty ->
    match Zzz.restart_threads sleep_q with
    | Some k -> Suspended.continue k ()                   (* A sleeping task is now due *)
    | None ->
      (* Handle any pending events before submitting. This is faster. *)
      match Uring.peek uring with
      | Some { data = runnable; result } -> handle_complete st ~runnable result
      | None ->
        let num_jobs = Uring.submit uring in
        st.io_jobs <- st.io_jobs + num_jobs;
        let timeout = Zzz.select_next sleep_q in
        Log.debug (fun l -> l "scheduler: %d sub / %d total, timeout %s" num_jobs st.io_jobs
                      (match timeout with None -> "inf" | Some v -> string_of_float v));
        assert (Queue.length run_q = 0);
        if timeout = None && st.io_jobs = 0 then (
          (* Nothing further can happen at this point.
             If there are no events in progress but also still no memory available, something has gone wrong! *)
          assert (Queue.length mem_q = 0);
          Log.debug (fun l -> l "schedule: exiting");    (* Nothing left to do *)
          `Exit_scheduler
        ) else (
          Ctf.(note_hiatus Wait_for_work);
          let result = Uring.wait ?timeout uring in
          Ctf.note_resume system_thread;
          match result with
          | None ->
            assert (timeout <> None);
            schedule st                                   (* Woken by a timeout, which is now due *)
          | Some { data = runnable; result } ->
            handle_complete st ~runnable result
        )
and handle_complete st ~runnable result =
  st.io_jobs <- st.io_jobs - 1;
  submit_pending_io st;                       (* If something was waiting for a slot, submit it now. *)
  match runnable with
  | Read (req, cancel) ->
    Log.debug (fun l -> l "read returned");
    Eio.Private.Waiters.remove_waiter !cancel;
    complete_rw_req st req result
  | Write (req, cancel) ->
    Log.debug (fun l -> l "write returned");
    Eio.Private.Waiters.remove_waiter !cancel;
    complete_rw_req st req result
  | Poll_add (k, cancel) ->
    Log.debug (fun l -> l "poll_add returned");
    Eio.Private.Waiters.remove_waiter !cancel;
    Suspended.continue k result
  | Splice (k, cancel) ->
    Log.debug (fun l -> l "splice returned");
    Eio.Private.Waiters.remove_waiter !cancel;
    Suspended.continue k result
  | Connect (k, cancel) ->
    Log.debug (fun l -> l "connect returned");
    Eio.Private.Waiters.remove_waiter !cancel;
    Suspended.continue k result
  | Accept (k, cancel) ->
    Log.debug (fun l -> l "accept returned");
    Eio.Private.Waiters.remove_waiter !cancel;
    Suspended.continue k result
  | Close k ->
    Log.debug (fun l -> l "close returned");
    Suspended.continue k result
  | Cancel k ->
    Log.debug (fun l -> l "cancel returned");
    Suspended.continue k result
and complete_rw_req st ({len; cur_off; action; _} as req) res =
  match res, len with
  | 0, _ -> Suspended.discontinue action End_of_file
  | e, _ when e < 0 ->
    if errno_is_retry e then (
      submit_rw_req st req;
      schedule st
    ) else (
      Suspended.continue action e
    )
  | n, Exactly len when n < len - cur_off ->
    req.cur_off <- req.cur_off + n;
    submit_rw_req st req;
    schedule st
  | _, Exactly len -> Suspended.continue action len
  | n, Upto _ -> Suspended.continue action n

let alloc_buf st k =
  Log.debug (fun l -> l "alloc: %d" (Uring.Region.avail st.mem));
  match Uring.Region.alloc st.mem with
  | buf -> Suspended.continue k buf 
  | exception Uring.Region.No_space ->
    Queue.push k st.mem_q;
    schedule st

let free_buf st buf =
  match Queue.take_opt st.mem_q with
  | None -> Uring.Region.free buf
  | Some k -> enqueue_thread st k buf

effect Sleep : float -> unit
let sleep d =
  perform (Sleep d)

effect ERead : (Switch.t option * Optint.Int63.t option * FD.t * Uring.Region.chunk * amount) -> int

let read_exactly ?sw ?file_offset fd buf len =
  let res = perform (ERead (sw, file_offset, fd, buf, Exactly len)) in
  Log.debug (fun l -> l "read_exactly: woken up after read");
  if res < 0 then (
    Option.iter Switch.check sw;    (* If cancelled, report that instead. *)
    raise (Unix.Unix_error (Uring.error_of_errno res, "read_exactly", ""))
  )

let read_upto ?sw ?file_offset fd buf len =
  let res = perform (ERead (sw, file_offset, fd, buf, Upto len)) in
  Log.debug (fun l -> l "read_upto: woken up after read");
  if res < 0 then (
    Option.iter Switch.check sw;    (* If cancelled, report that instead. *)
    raise (Unix.Unix_error (Uring.error_of_errno res, "read_upto", ""))
  ) else (
    res
  )

effect EPoll_add : Switch.t option * FD.t * Uring.Poll_mask.t -> int

let await_readable ?sw fd =
  let res = perform (EPoll_add (sw, fd, Uring.Poll_mask.(pollin + pollerr))) in
  Log.debug (fun l -> l "await_readable: woken up");
  if res < 0 then (
    Option.iter Switch.check sw;    (* If cancelled, report that instead. *)
    raise (Unix.Unix_error (Uring.error_of_errno res, "await_readable", ""))
  )

let await_writable ?sw fd =
  let res = perform (EPoll_add (sw, fd, Uring.Poll_mask.(pollout + pollerr))) in
  Log.debug (fun l -> l "await_writable: woken up");
  if res < 0 then (
    Option.iter Switch.check sw;    (* If cancelled, report that instead. *)
    raise (Unix.Unix_error (Uring.error_of_errno res, "await_writable", ""))
  )

effect EWrite : (Switch.t option * Optint.Int63.t option * FD.t * Uring.Region.chunk * amount) -> int

let write ?sw ?file_offset fd buf len =
  let res = perform (EWrite (sw, file_offset, fd, buf, Exactly len)) in
  Log.debug (fun l -> l "write: woken up after write");
  if res < 0 then (
    Option.iter Switch.check sw;    (* If cancelled, report that instead. *)
    raise (Unix.Unix_error (Uring.error_of_errno res, "write", ""))
  )

effect Alloc : Uring.Region.chunk
let alloc () = perform Alloc

effect Free : Uring.Region.chunk -> unit
let free buf = perform (Free buf)

effect Splice : Switch.t option * FD.t * FD.t * int -> int
let splice ?sw src ~dst ~len =
  let res = perform (Splice (sw, src, dst, len)) in
  if res > 0 then res
  else if res = 0 then raise End_of_file
  else (
    Option.iter Switch.check sw;    (* If cancelled, report that instead. *)
    raise (Unix.Unix_error (Uring.error_of_errno res, "splice", ""))
  )

effect Connect : Switch.t option * FD.t * Unix.sockaddr -> int
let connect ?sw fd addr =
  let res = perform (Connect (sw, fd, addr)) in
  if res < 0 then (
    Option.iter Switch.check sw;    (* If cancelled, report that instead. *)
    raise (Unix.Unix_error (Uring.error_of_errno res, "connect", ""))
  )

let with_chunk fn =
  let chunk = alloc () in
  Fun.protect ~finally:(fun () -> free chunk) @@ fun () ->
  fn chunk

let openfile ~sw path flags mode =
  let fd = Unix.openfile path flags mode in
  FD.of_unix ~sw ~seekable:(FD.is_seekable fd) fd

let fstat fd =
  Unix.fstat (FD.get "fstat" fd)

let shutdown socket command =
  Unix.shutdown (FD.get "shutdown" socket) command

effect Accept : Switch.t * FD.t * Uring.Sockaddr.t -> int
let accept_loose_fd ~sw socket =
  Ctf.label "accept";
  let client_addr = Uring.Sockaddr.create () in
  let res = perform (Accept (sw, socket, client_addr)) in
  if res < 0 then (
    Switch.check sw;    (* If cancelled, report that instead. *)
    raise (Unix.Unix_error (Uring.error_of_errno res, "accept", ""))
  ) else (
    let unix : Unix.file_descr = Obj.magic res in
    let fd = FD.of_unix_no_hook ~seekable:false unix in
    let addr = Uring.Sockaddr.get client_addr in
    fd, addr
  )

let accept ~sw fd =
  let client, client_addr = accept_loose_fd ~sw fd in
  Switch.on_release sw (fun () -> FD.ensure_closed client);
  client, client_addr

module Objects = struct
  type _ Eio.Generic.ty += FD : FD.t Eio.Generic.ty

  type has_fd = < fd : FD.t >
  type source = < Eio.Flow.source; Eio.Flow.close; has_fd >
  type sink   = < Eio.Flow.sink  ; Eio.Flow.close; has_fd >

  let get_fd (t : <has_fd; ..>) = t#fd

  let get_fd_opt t = Eio.Generic.probe t FD

  (* When copying between a source with an FD and a sink with an FD, we can share the chunk
     and avoid copying. *)
  let fast_copy ?sw src dst =
    with_chunk @@ fun chunk ->
    let chunk_size = Uring.Region.length chunk in
    try
      while true do
        let got = read_upto ?sw src chunk chunk_size in
        write dst chunk got
      done
    with End_of_file -> ()

  (* Try a fast copy using splice. If the FDs don't support that, switch to copying. *)
  let fast_copy_try_splice ?sw src dst =
    try
      while true do
        let _ : int = splice ?sw src ~dst ~len:max_int in
        ()
      done
    with
    | End_of_file -> ()
    | Unix.Unix_error (Unix.EINVAL, "splice", _) -> fast_copy ?sw src dst

  let flow fd = object (_ : <source; sink; ..>)
    method fd = fd
    method close = FD.close fd

    method probe : type a. a Eio.Generic.ty -> a option = function
      | FD -> Some fd
      | _ -> None

    method read_into ?sw buf =
      (* Inefficient copying fallback *)
      with_chunk @@ fun chunk ->
      let chunk_cs = Cstruct.of_bigarray (Uring.Region.to_bigstring chunk) in
      let max_len = min (Cstruct.length buf) (Cstruct.length chunk_cs) in
      let got = read_upto ?sw fd chunk max_len in
      Cstruct.blit chunk_cs 0 buf 0 got;
      got

    method write ?sw src =
      match get_fd_opt src with
      | Some src -> fast_copy_try_splice ?sw src fd
      | None ->
        (* Inefficient copying fallback *)
        with_chunk @@ fun chunk ->
        let chunk_cs = Cstruct.of_bigarray (Uring.Region.to_bigstring chunk) in
        try
          while true do
            let got = Eio.Flow.read_into ?sw src chunk_cs in
            write fd chunk got
          done
        with End_of_file -> ()

    method shutdown cmd =
      Unix.shutdown (FD.get "shutdown" fd) cmd
  end

  let source fd = (flow fd :> source)
  let sink   fd = (flow fd :> sink)

  let listening_socket fd = object
    inherit Eio.Network.Listening_socket.t

    method close = FD.close fd

    method listen n = Unix.listen (FD.get "listen" fd) n

    method accept_sub ~sw ~on_error fn =
      let client, client_addr = accept_loose_fd ~sw fd in
      Fibre.fork_sub_ignore ~sw ~on_error
        (fun sw -> fn ~sw (flow client :> <Eio.Flow.two_way; Eio.Flow.close>) client_addr)
        ~on_release:(fun () -> FD.ensure_closed client)
  end

  let network = object
    inherit Eio.Network.t

    method bind ~reuse_addr ~sw addr =
      let sock_unix = Unix.(socket PF_INET SOCK_STREAM 0) in
      if reuse_addr then
        Unix.setsockopt sock_unix Unix.SO_REUSEADDR true;
      let sock = FD.of_unix ~sw ~seekable:false sock_unix in
      Unix.bind sock_unix addr;
      listening_socket sock

    method connect ~sw addr =
      let sock_unix = Unix.(socket PF_INET SOCK_STREAM 0) in
      let sock = FD.of_unix ~sw ~seekable:false sock_unix in
      connect ~sw sock addr;
      (flow sock :> <Eio.Flow.two_way; Eio.Flow.close>)
  end

  type stdenv = <
    stdin  : source;
    stdout : sink;
    stderr : sink;
    network : Eio.Network.t;
  >

  let stdenv () =
    let of_unix fd = FD.of_unix_no_hook ~seekable:(FD.is_seekable fd) fd in
    let stdin = lazy (source (of_unix Unix.stdin)) in
    let stdout = lazy (sink (of_unix Unix.stdout)) in
    let stderr = lazy (sink (of_unix Unix.stderr)) in
    object (_ : stdenv)
      method stdin  = Lazy.force stdin
      method stdout = Lazy.force stdout
      method stderr = Lazy.force stderr
      method network = network
    end
end

let pipe sw =
  let r, w = Unix.pipe () in
  let r = Objects.source (FD.of_unix ~sw ~seekable:false r) in
  let w = Objects.sink (FD.of_unix ~sw ~seekable:false w) in
  r, w

let run ?(queue_depth=64) ?(block_size=4096) main =
  Log.debug (fun l -> l "starting run");
  let stdenv = Objects.stdenv () in
  (* TODO unify this allocation API around baregion/uring *)
  let fixed_buf_len = block_size * queue_depth in
  let uring = Uring.create ~fixed_buf_len ~queue_depth () in
  let buf = Uring.buf uring in 
  let mem = Uring.Region.init ~block_size buf queue_depth in
  let run_q = Queue.create () in
  let sleep_q = Zzz.init () in
  let io_q = Queue.create () in
  let mem_q = Queue.create () in
  let st = { mem; uring; run_q; io_q; mem_q; sleep_q; io_jobs = 0 } in
  Log.debug (fun l -> l "starting main thread");
  let rec fork ~tid fn =
    Ctf.note_switch tid;
    match fn () with
    | () -> schedule st
    | effect (ERead args) k ->
      let k = { Suspended.k; tid } in
      enqueue_read st k args;
      schedule st
    | effect (EPoll_add (sw, fd, poll_mask)) k ->
      let k = { Suspended.k; tid } in
      enqueue_poll_add ?sw st k fd poll_mask;
      schedule st
    | effect (Close fd) k ->
      let k = { Suspended.k; tid } in
      enqueue_close st k fd;
      schedule st
    | effect (EWrite args) k ->
      let k = { Suspended.k; tid } in
      enqueue_write st k args;
      schedule st
    | effect (Splice (sw, src, dst, len)) k ->
      let k = { Suspended.k; tid } in
      enqueue_splice ?sw st k ~src ~dst ~len;
      schedule st
    | effect (Connect (sw, fd, addr)) k ->
      let k = { Suspended.k; tid } in
      enqueue_connect ?sw st k fd addr;
      schedule st
    | effect (Accept (sw, fd, client_addr)) k ->
      let k = { Suspended.k; tid } in
      enqueue_accept ~sw st k fd client_addr;
      schedule st
    | effect Eio.Private.Effects.Yield k ->
      let k = { Suspended.k; tid } in
      enqueue_thread st k ();
      schedule st
    | effect (Sleep d) k ->
      let k = { Suspended.k; tid } in
      Zzz.sleep sleep_q d k;
      schedule st
    | effect (Eio.Private.Effects.Await (sw, pid, q)) k ->
      let k = { Suspended.k; tid } in
      let waiters = Queue.create () in
      let when_resolved r =
        Queue.iter Eio.Private.Waiters.remove_waiter waiters;
        match r with
        | Ok v ->
          Ctf.note_read ~reader:tid pid;
          enqueue_thread st k v
        | Error ex ->
          Ctf.note_read ~reader:tid pid;
          enqueue_failed_thread st k ex
      in
      let cancel ex = when_resolved (Error ex) in
      sw |> Option.iter (fun sw ->
          let cancel_waiter = Eio.Private.Switch.add_cancel_hook sw cancel in
          Queue.add cancel_waiter waiters;
        );
      let resolved_waiter = Eio.Private.Waiters.add_waiter q when_resolved in
      Queue.add resolved_waiter waiters;
      schedule st
    | effect (Eio.Private.Effects.Fork f) k ->
      let k = { Suspended.k; tid } in
      let id = Ctf.mint_id () in
      Ctf.note_created id Ctf.Task;
      let promise, resolver = Promise.create_with_id id in
      enqueue_thread st k promise;
      fork
        ~tid:id
        (fun () ->
           match f () with
           | x -> Promise.fulfill resolver x
           | exception ex ->
             Log.debug (fun f -> f "Forked fibre failed: %a" Fmt.exn ex);
             Promise.break resolver ex
        )
    | effect (Eio.Private.Effects.Fork_ignore f) k ->
      let k = { Suspended.k; tid } in
      enqueue_thread st k ();
      let child = Ctf.note_fork () in
      Ctf.note_switch child;
      fork ~tid:child (fun () ->
          match f () with
          | () ->
            Ctf.note_resolved child ~ex:None
          | exception ex ->
            Ctf.note_resolved child ~ex:(Some ex)
        )
    | effect Alloc k ->
      let k = { Suspended.k; tid } in
      alloc_buf st k
    | effect (Free buf) k ->
      free_buf st buf;
      continue k ()
    | effect (Cancel job) k ->
      let k = { Suspended.k; tid } in
      enqueue_cancel st k job;
      schedule st
  in
  let main_done = ref false in
  let `Exit_scheduler = fork ~tid:(Ctf.mint_id ()) (fun () ->
      Fun.protect (fun () -> main stdenv)
        ~finally:(fun () -> main_done := true)
  ) in
  if not !main_done then
    failwith "Deadlock detected: no events scheduled but main function hasn't returned";
  Log.debug (fun l -> l "exit")
