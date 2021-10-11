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

let src = Logs.Src.create "eio_linux" ~doc:"Effect-based IO system for Linux/io-uring"
module Log = (val Logs.src_log src : Logs.LOG)

open Eio.Std
open Obj.Effect_handlers
open Obj.Effect_handlers.Deep 

module Suspended = Eunix.Suspended
module Zzz = Eunix.Zzz

(* SIGPIPE makes no sense in a modern application. *)
let () = Sys.(set_signal sigpipe Signal_ignore)

type amount = Exactly of int | Upto of int

let system_thread = Ctf.mint_id ()

let wrap_errors path fn =
  try fn () with
  | Unix.Unix_error(Unix.EEXIST, _, _) as ex -> raise @@ Eio.Dir.Already_exists (path, ex)
  | Unix.Unix_error(Unix.ENOENT, _, _) as ex -> raise @@ Eio.Dir.Not_found (path, ex)
  | Unix.Unix_error(Unix.EXDEV, _, _)  as ex -> raise @@ Eio.Dir.Permission_denied (path, ex)
  | Eio.Dir.Permission_denied _        as ex -> raise @@ Eio.Dir.Permission_denied (path, ex)

type _ eff += Close : Unix.file_descr -> int eff

module FD = struct
  type t = {
    seekable : bool;
    mutable release_hook : Switch.hook;        (* Use this on close to remove switch's [on_release] hook. *)
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
    Switch.remove_hook t.release_hook;
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
    { seekable; fd = `Open fd; release_hook = Switch.null_hook }

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

type cancel_hook = Switch.hook ref

(* Type of user-data attached to jobs. *)
type io_job =
  | Read : rw_req * cancel_hook -> io_job
  | Job_no_cancel : int Suspended.t -> io_job
  | Job : int Suspended.t * cancel_hook -> io_job
  | Write : rw_req * cancel_hook -> io_job

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

type _ eff += Enter : (t -> 'a Suspended.t -> unit) -> 'a eff
let enter fn = perform (Enter fn)

let rec enqueue_cancel job st action =
  Log.debug (fun l -> l "cancel: submitting call");
  Ctf.label "cancel";
  match Uring.cancel st.uring job (Job_no_cancel action) with
  | None -> Queue.push (fun st -> enqueue_cancel job st action) st.io_q
  | Some _ -> ()

let cancel job =
  let res = enter (enqueue_cancel job) in
  Log.debug (fun l -> l "cancel returned");
  if res = -2 then (
    Log.debug (fun f -> f "Cancel returned ENOENT - operation completed before cancel took effect")
  ) else if res = -114 then (
    Log.debug (fun f -> f "Cancel returned EALREADY - operation cancelled while already in progress")
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
  let release = ref Switch.null_hook in
  match sw with
  | None -> fn release = None
  | Some sw ->
    match Switch.get_error sw with
    | Some ex -> enqueue_failed_thread st action ex; false
    | None ->
      match fn release with
      | None -> true
      | Some job ->
        release := Switch.add_cancel_hook sw (fun _ -> cancel job);
        false

let rec submit_rw_req st ({op; file_offset; fd; buf; len; cur_off; sw; action} as req) =
  let fd = FD.get "submit_rw_req" fd in
  let {uring;io_q;_} = st in
  let off = Uring.Region.to_offset buf + cur_off in
  let len = match len with Exactly l | Upto l -> l in
  let len = len - cur_off in
  let retry = with_cancel_hook ?sw ~action st (fun cancel ->
      match op with
      |`R -> Uring.read_fixed uring ~file_offset fd ~off ~len (Read (req, cancel))
      |`W -> Uring.write_fixed uring ~file_offset fd ~off ~len (Write (req, cancel))
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

let rec enqueue_readv args st action =
  let (sw,file_offset,fd,bufs) = args in
  let file_offset =
    match file_offset with
    | Some x -> x
    | None -> FD.uring_file_offset fd
  in
  Ctf.label "readv";
  let retry = with_cancel_hook ?sw ~action st (fun cancel ->
      Uring.readv st.uring ~file_offset (FD.get "readv" fd) bufs (Job (action, cancel))
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_readv args st action) st.io_q

let rec enqueue_writev args st action =
  let (sw,file_offset,fd,bufs) = args in
  let file_offset =
    match file_offset with
    | Some x -> x
    | None -> FD.uring_file_offset fd
  in
  Ctf.label "writev";
  let retry = with_cancel_hook ?sw ~action st (fun cancel ->
      Uring.writev st.uring ~file_offset (FD.get "writev" fd) bufs (Job (action, cancel))
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_writev args st action) st.io_q

let rec enqueue_poll_add ?sw fd poll_mask st action =
  Log.debug (fun l -> l "poll_add: submitting call");
  Ctf.label "poll_add";
  let retry = with_cancel_hook ?sw ~action st (fun cancel ->
      Uring.poll_add st.uring (FD.get "poll_add" fd) poll_mask (Job (action, cancel))
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_poll_add ?sw fd poll_mask st action) st.io_q

let rec enqueue_close st action fd =
  Log.debug (fun l -> l "close: submitting call");
  Ctf.label "close";
  let subm = Uring.close st.uring fd (Job_no_cancel action) in
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

let rec enqueue_splice ?sw ~src ~dst ~len st action =
  Log.debug (fun l -> l "splice: submitting call");
  Ctf.label "splice";
  let retry = with_cancel_hook ?sw ~action st (fun cancel ->
      Uring.splice st.uring (Job (action, cancel)) ~src:(FD.get "splice" src) ~dst:(FD.get "splice" dst) ~len
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_splice ?sw ~src ~dst ~len st action) st.io_q

let rec enqueue_openat2 ((sw, access, flags, perm, resolve, dir, path) as args) st action =
  Log.debug (fun l -> l "openat2: submitting call");
  Ctf.label "openat2";
  let fd = Option.map (FD.get "openat2") dir in
  let retry = with_cancel_hook ~sw ~action st (fun cancel ->
      Uring.openat2 st.uring ~access ~flags ~perm ~resolve ?fd path (Job (action, cancel))
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_openat2 args st action) st.io_q

let rec enqueue_connect ?sw fd addr st action =
  Log.debug (fun l -> l "connect: submitting call");
  Ctf.label "connect";
  let retry = with_cancel_hook ?sw ~action st (fun cancel ->
      Uring.connect st.uring (FD.get "connect" fd) addr (Job (action, cancel))
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_connect ?sw fd addr st action) st.io_q

let rec enqueue_accept ~sw fd client_addr st action =
  Log.debug (fun l -> l "accept: submitting call");
  Ctf.label "accept";
  let retry = with_cancel_hook ~sw ~action st (fun cancel ->
      Uring.accept st.uring (FD.get "accept" fd) client_addr (Job (action, cancel))
    ) in
  if retry then (
    (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_accept ~sw fd client_addr st action) st.io_q
  )

let rec enqueue_noop st action =
  Log.debug (fun l -> l "noop: submitting call");
  Ctf.label "noop";
  let retry = (Uring.noop st.uring (Job_no_cancel action) = None) in
  if retry then (
    (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_noop st action) st.io_q
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
    let now = Unix.gettimeofday () in
    match Zzz.pop ~now sleep_q with
    | `Due k -> Suspended.continue k ()                   (* A sleeping task is now due *)
    | `Wait_until _ | `Nothing as next_due ->
      (* Handle any pending events before submitting. This is faster. *)
      match Uring.peek uring with
      | Some { data = runnable; result } -> handle_complete st ~runnable result
      | None ->
        let num_jobs = Uring.submit uring in
        st.io_jobs <- st.io_jobs + num_jobs;
        let timeout =
          match next_due with
          | `Wait_until time -> Some (time -. now)
          | `Nothing -> None
        in
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
            (* Woken by a timeout, which is now due, or by a signal. *)
            schedule st
          | Some { data = runnable; result } ->
            handle_complete st ~runnable result
        )
and handle_complete st ~runnable result =
  st.io_jobs <- st.io_jobs - 1;
  submit_pending_io st;                       (* If something was waiting for a slot, submit it now. *)
  match runnable with
  | Read (req, cancel) ->
    Log.debug (fun l -> l "read returned");
    Switch.remove_hook !cancel;
    complete_rw_req st req result
  | Write (req, cancel) ->
    Log.debug (fun l -> l "write returned");
    Switch.remove_hook !cancel;
    complete_rw_req st req result
  | Job (k, cancel) ->
    Switch.remove_hook !cancel;
    Suspended.continue k result
  | Job_no_cancel k ->
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

let noop () =
  let result = enter enqueue_noop in
  Log.debug (fun l -> l "noop returned");
  if result <> 0 then raise (Unix.Unix_error (Uring.error_of_errno result, "noop", ""))

type _ eff += Sleep_until : Switch.t option * float -> unit eff
let sleep_until ?sw d =
  perform (Sleep_until (sw, d))

type _ eff += ERead : (Switch.t option * Optint.Int63.t option * FD.t * Uring.Region.chunk * amount) -> int eff

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
    let err = Uring.error_of_errno res in
    let ex = Unix.Unix_error (err, "read_upto", "") in
    if err = Unix.ECONNRESET then raise (Eio.Net.Connection_reset ex)
    else raise ex
  ) else (
    res
  )

let readv ?sw ?file_offset fd bufs =
  let res = enter (enqueue_readv (sw, file_offset, fd, bufs)) in
  Log.debug (fun l -> l "readv: woken up after read");
  if res < 0 then (
    Option.iter Switch.check sw;    (* If cancelled, report that instead. *)
    raise (Unix.Unix_error (Uring.error_of_errno res, "readv", ""))
  ) else if res = 0 then (
    raise End_of_file
  ) else (
    res
  )

let rec writev ?sw ?file_offset fd bufs =
  let res = enter (enqueue_writev (sw, file_offset, fd, bufs)) in
  Log.debug (fun l -> l "writev: woken up after write");
  if res < 0 then (
    Option.iter Switch.check sw;    (* If cancelled, report that instead. *)
    raise (Unix.Unix_error (Uring.error_of_errno res, "writev", ""))
  ) else (
    match Cstruct.shiftv bufs res with
    | [] -> ()
    | bufs ->
      let file_offset =
        let module I63 = Optint.Int63 in
        match file_offset with
        | None -> None
        | Some ofs when ofs = I63.minus_one -> Some I63.minus_one
        | Some ofs -> Some (I63.add ofs (I63.of_int res))
      in
      writev ?sw ?file_offset fd bufs
  )

let await_readable ?sw fd =
  let res = enter (enqueue_poll_add ?sw fd (Uring.Poll_mask.(pollin + pollerr))) in
  Log.debug (fun l -> l "await_readable: woken up");
  if res < 0 then (
    Option.iter Switch.check sw;    (* If cancelled, report that instead. *)
    raise (Unix.Unix_error (Uring.error_of_errno res, "await_readable", ""))
  )

let await_writable ?sw fd =
  let res = enter (enqueue_poll_add ?sw fd (Uring.Poll_mask.(pollout + pollerr))) in
  Log.debug (fun l -> l "await_writable: woken up");
  if res < 0 then (
    Option.iter Switch.check sw;    (* If cancelled, report that instead. *)
    raise (Unix.Unix_error (Uring.error_of_errno res, "await_writable", ""))
  )

type _ eff += EWrite : (Switch.t option * Optint.Int63.t option * FD.t * Uring.Region.chunk * amount) -> int eff

let write ?sw ?file_offset fd buf len =
  let res = perform (EWrite (sw, file_offset, fd, buf, Exactly len)) in
  Log.debug (fun l -> l "write: woken up after write");
  if res < 0 then (
    Option.iter Switch.check sw;    (* If cancelled, report that instead. *)
    raise (Unix.Unix_error (Uring.error_of_errno res, "write", ""))
  )

type _ eff += Alloc : Uring.Region.chunk eff
let alloc () = perform Alloc

type _ eff += Free : Uring.Region.chunk -> unit eff
let free buf = perform (Free buf)

let splice ?sw src ~dst ~len =
  let res = enter (enqueue_splice ?sw ~src ~dst ~len) in
  Log.debug (fun l -> l "splice returned");
  if res > 0 then res
  else if res = 0 then raise End_of_file
  else (
    Option.iter Switch.check sw;    (* If cancelled, report that instead. *)
    raise (Unix.Unix_error (Uring.error_of_errno res, "splice", ""))
  )

let connect ?sw fd addr =
  let res = enter (enqueue_connect ?sw fd addr) in
  Log.debug (fun l -> l "connect returned");
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

let openat2 ~sw ?seekable ~access ~flags ~perm ~resolve ?dir path =
  wrap_errors path @@ fun () ->
  let res = enter (enqueue_openat2 (sw, access, flags, perm, resolve, dir, path)) in
  Log.debug (fun l -> l "openat2 returned");
  if res < 0 then (
    Switch.check sw;    (* If cancelled, report that instead. *)
    raise @@ Unix.Unix_error (Uring.error_of_errno res, "openat2", "")
  );
  let fd : Unix.file_descr = Obj.magic res in
  let seekable =
    match seekable with
    | None -> FD.is_seekable fd
    | Some x -> x
  in
  FD.of_unix ~sw ~seekable fd

let fstat fd =
  Unix.fstat (FD.get "fstat" fd)

external eio_mkdirat : Unix.file_descr -> string -> Unix.file_perm -> unit = "caml_eio_mkdirat"

(* We ignore [sw] because this isn't a uring operation yet. *)
let mkdirat ?sw:_ ~perm dir path =
  wrap_errors path @@ fun () ->
  match dir with
  | None -> Unix.mkdir path perm
  | Some dir -> eio_mkdirat (FD.get "mkdirat" dir) path perm

let mkdir_beneath ?sw ~perm ?dir path =
  let dir_path = Filename.dirname path in
  let leaf = Filename.basename path in
  (* [mkdir] is really an operation on [path]'s parent. Get a reference to that first: *)
  Switch.sub_opt sw (fun sw ->
      let parent =
        wrap_errors path @@ fun () ->
        openat2 ~sw ~seekable:false ?dir dir_path
          ~access:`R
          ~flags:Uring.Open_flags.(cloexec + path + directory)
          ~perm:0
          ~resolve:Uring.Resolve.beneath
      in
      mkdirat ~sw ~perm (Some parent) leaf
    )

let shutdown socket command =
  Unix.shutdown (FD.get "shutdown" socket) command

let accept_loose_fd ~sw socket =
  Ctf.label "accept";
  let client_addr = Uring.Sockaddr.create () in
  let res = enter (enqueue_accept ~sw socket client_addr) in
  Log.debug (fun l -> l "accept returned");
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

let run_compute fn () =
  match_with fn ()
  { retc = (fun x -> x);
    exnc = (fun e -> raise e);
    effc = fun (type a) (e : a eff) -> 
          match e with
          | Eio.Private.Effects.Trace -> 
            Some (fun (k: (a, _) continuation) -> continue k Eunix.Trace.default_traceln)
          | _ -> None
  }

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
        write ?sw dst chunk got
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

  (* Copy using the [Read_source_buffer] optimisation.
     Avoids a copy if the source already has the data. *)
  let copy_with_rsb ?sw rsb dst =
    try
      while true do
        rsb ?sw (writev ?sw dst)
      done
    with End_of_file -> ()

  (* Copy by allocating a chunk from the pre-shared buffer and asking
     the source to write into it. This used when the other methods
     aren't available. *)
  let fallback_copy ?sw src dst =
    with_chunk @@ fun chunk ->
    let chunk_cs = Uring.Region.to_cstruct chunk in
    try
      while true do
        let got = Eio.Flow.read_into ?sw src chunk_cs in
        write ?sw dst chunk got
      done
    with End_of_file -> ()

  let flow fd =
    let is_tty = lazy (Unix.isatty (FD.get "isatty" fd)) in
    object (_ : <source; sink; ..>)
      method fd = fd
      method close = FD.close fd

      method probe : type a. a Eio.Generic.ty -> a option = function
        | FD -> Some fd
        | _ -> None

      method read_into ?sw buf =
        (* Inefficient copying fallback *)
        with_chunk @@ fun chunk ->
        let chunk_cs = Uring.Region.to_cstruct chunk in
        let max_len = min (Cstruct.length buf) (Cstruct.length chunk_cs) in
        if Lazy.force is_tty then (
          (* Work-around for https://github.com/axboe/liburing/issues/354
             (should be fixed in Linux 5.14) *)
          await_readable ?sw fd
        );
        let got = read_upto ?sw fd chunk max_len in
        Cstruct.blit chunk_cs 0 buf 0 got;
        got

      method read_methods = []

      method write ?sw src =
        match get_fd_opt src with
        | Some src -> fast_copy_try_splice ?sw src fd
        | None ->
          let rec aux = function
            | Eio.Flow.Read_source_buffer rsb :: _ -> copy_with_rsb ?sw rsb fd
            | _ :: xs -> aux xs
            | [] -> fallback_copy ?sw src fd
          in
          aux (Eio.Flow.read_methods src)

      method shutdown cmd =
        Unix.shutdown (FD.get "shutdown" fd) @@ match cmd with
        | `Receive -> Unix.SHUTDOWN_RECEIVE
        | `Send -> Unix.SHUTDOWN_SEND
        | `All -> Unix.SHUTDOWN_ALL
    end

  let source fd = (flow fd :> source)
  let sink   fd = (flow fd :> sink)

  let listening_socket fd = object
    inherit Eio.Net.listening_socket

    method close = FD.close fd

    method accept_sub ~sw ~on_error fn =
      let client, client_addr = accept_loose_fd ~sw fd in
      Fibre.fork_sub_ignore ~sw ~on_error
        (fun sw ->
           let client_addr = match client_addr with
             | Unix.ADDR_UNIX path         -> `Unix path
             | Unix.ADDR_INET (host, port) -> `Tcp (host, port)
           in
           fn ~sw (flow client :> <Eio.Flow.two_way; Eio.Flow.close>) client_addr
        )
        ~on_release:(fun () -> FD.ensure_closed client)
  end

  let net = object
    inherit Eio.Net.t

    method listen ~reuse_addr ~backlog ~sw listen_addr =
      let socket_domain, socket_type, addr =
        match listen_addr with
        | `Unix path         ->
          if reuse_addr then (
            match Unix.lstat path with
            | Unix.{ st_kind = S_SOCK; _ } -> Unix.unlink path
            | _ -> ()
            | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
          );
          Unix.PF_UNIX, Unix.SOCK_STREAM, Unix.ADDR_UNIX path
        | `Tcp (host, port)  -> Unix.PF_INET, Unix.SOCK_STREAM, Unix.ADDR_INET (host, port)
      in
      let sock_unix = Unix.socket socket_domain socket_type 0 in
      (* For Unix domain sockets, remove the path when done (except for abstract sockets). *)
      begin match listen_addr with
        | `Unix path ->
          if String.length path > 0 && path.[0] <> Char.chr 0 then
            Switch.on_release sw (fun () -> Unix.unlink path)
        | `Tcp _ -> ()
      end;
      if reuse_addr then
        Unix.setsockopt sock_unix Unix.SO_REUSEADDR true;
      let sock = FD.of_unix ~sw ~seekable:false sock_unix in
      Unix.bind sock_unix addr;
      Unix.listen sock_unix backlog;
      listening_socket sock

    method connect ~sw addr =
      let socket_domain, socket_type, addr =
        match addr with
        | `Unix path         -> Unix.PF_UNIX, Unix.SOCK_STREAM, Unix.ADDR_UNIX path
        | `Tcp (host, port)  -> Unix.PF_INET, Unix.SOCK_STREAM, Unix.ADDR_INET (host, port)
      in
      let sock_unix = Unix.socket socket_domain socket_type 0 in
      let sock = FD.of_unix ~sw ~seekable:false sock_unix in
      connect ~sw sock addr;
      (flow sock :> <Eio.Flow.two_way; Eio.Flow.close>)
  end

  type stdenv = <
    stdin  : source;
    stdout : sink;
    stderr : sink;
    net : Eio.Net.t;
    domain_mgr : Eio.Domain_manager.t;
    clock : Eio.Time.clock;
    fs : Eio.Dir.t;
    cwd : Eio.Dir.t;
  >

  let domain_mgr = object
    inherit Eio.Domain_manager.t

    method run_compute_unsafe fn =
      (* todo: use eventfd instead of a pipe *)
      let r, w = Unix.pipe () in
      let r = FD.of_unix_no_hook ~seekable:false r in
      match Domain.spawn (fun () -> Fun.protect (run_compute fn) ~finally:(fun () -> Unix.close w)) with
      | domain ->
        await_readable r;
        FD.close r;
        Domain.join domain
      | exception ex ->
        Unix.close w;
        FD.close r;
        raise ex
  end

  let clock = object
    inherit Eio.Time.clock

    method now = Unix.gettimeofday ()
    method sleep_until = sleep_until
  end

  class dir fd = object
    inherit Eio.Dir.t

    val resolve_flags = Uring.Resolve.beneath

    method open_in ~sw path =
      let fd = openat2 ~sw ?dir:fd path
          ~access:`R
          ~flags:Uring.Open_flags.cloexec
          ~perm:0
          ~resolve:Uring.Resolve.beneath
      in
      (flow fd :> <Eio.Flow.source; Eio.Flow.close>)

    method open_out ~sw ~append ~create path =
      let perm, flags =
        match create with
        | `Never            -> 0,    Uring.Open_flags.empty
        | `If_missing  perm -> perm, Uring.Open_flags.creat
        | `Or_truncate perm -> perm, Uring.Open_flags.(creat + trunc)
        | `Exclusive   perm -> perm, Uring.Open_flags.(creat + excl)
      in
      let flags = if append then Uring.Open_flags.(flags + append) else flags in
      let fd = openat2 ~sw ?dir:fd path
          ~access:`RW
          ~flags:Uring.Open_flags.(cloexec + flags)
          ~perm
          ~resolve:resolve_flags
      in
      (flow fd :> <Eio.Dir.rw; Eio.Flow.close>)

    method open_dir ~sw path =
      let fd = openat2 ~sw ~seekable:false ?dir:fd path
          ~access:`R
          ~flags:Uring.Open_flags.(cloexec + path + directory)
          ~perm:0
          ~resolve:resolve_flags
      in
      (new dir (Some fd) :> <Eio.Dir.t; Eio.Flow.close>)

    method mkdir ?sw ~perm path =
      mkdir_beneath ?sw ~perm ?dir:fd path

    method close =
      FD.close (Option.get fd)
  end

  (* Full access to the filesystem. *)
  let fs = object
    inherit dir None

    val! resolve_flags = Uring.Resolve.empty

    method! mkdir ?sw ~perm path =
      mkdirat ?sw ~perm None path
  end

  let stdenv () =
    let of_unix fd = FD.of_unix_no_hook ~seekable:(FD.is_seekable fd) fd in
    let stdin = lazy (source (of_unix Unix.stdin)) in
    let stdout = lazy (sink (of_unix Unix.stdout)) in
    let stderr = lazy (sink (of_unix Unix.stderr)) in
    let cwd = new dir None in
    object (_ : stdenv)
      method stdin  = Lazy.force stdin
      method stdout = Lazy.force stdout
      method stderr = Lazy.force stderr
      method net = net
      method domain_mgr = domain_mgr
      method clock = clock
      method fs = (fs :> Eio.Dir.t)
      method cwd = (cwd :> Eio.Dir.t)
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
  Fun.protect ~finally:(fun () -> Uring.exit uring) @@ fun () ->
  let buf = Uring.buf uring in
  let mem = Uring.Region.init ~block_size buf queue_depth in
  let run_q = Queue.create () in
  let sleep_q = Zzz.create () in
  let io_q = Queue.create () in
  let mem_q = Queue.create () in
  let st = { mem; uring; run_q; io_q; mem_q; sleep_q; io_jobs = 0 } in
  Log.debug (fun l -> l "starting main thread");
  let rec fork ~tid fn =
    Ctf.note_switch tid;
    match_with fn () 
    { retc = (fun () -> schedule st);
      exnc = (fun e -> raise e);
      effc = fun (type a) (e : a eff) ->  
        match e with 
        | Enter fn ->
          Some (fun k ->
            let k = { Suspended.k; tid } in
            fn st k;
            schedule st)
        | ERead args ->
          Some (fun k -> 
            let k = { Suspended.k; tid } in
            enqueue_read st k args;
            schedule st)
        | Close fd ->
          Some (fun k -> 
            let k = { Suspended.k; tid } in
            enqueue_close st k fd;
            schedule st)
        | EWrite args ->
          Some (fun k -> 
            let k = { Suspended.k; tid } in
            enqueue_write st k args;
            schedule st)
        | Sleep_until (sw, time) ->
          Some (fun k -> 
            let k = { Suspended.k; tid } in
            let cancel_hook = ref Switch.null_hook in
            begin match sw with
              | None ->
                ignore (Zzz.add ~cancel_hook sleep_q time k : Zzz.Key.t);
                schedule st
              | Some sw ->
                match Switch.get_error sw with
                | Some ex -> Suspended.discontinue k ex
                | None ->
                  let job = Zzz.add ~cancel_hook sleep_q time k in
                  cancel_hook := Switch.add_cancel_hook sw (fun ex ->
                      Zzz.remove sleep_q job;
                      enqueue_failed_thread st k ex
                    );
                  schedule st
            end)
        | Eio.Private.Effects.Suspend f ->
          Some (fun k -> 
            let k = { Suspended.k; tid } in
            f tid (function
                | Ok v -> enqueue_thread st k v
                | Error ex -> enqueue_failed_thread st k ex
              );
            schedule st)
        | Eio.Private.Effects.Fork f ->
          Some (fun k -> 
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
              ))
        | Eio.Private.Effects.Fork_ignore f ->
          Some (fun k -> 
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
              ))
        | Eio.Private.Effects.Trace -> Some (fun k -> continue k Eunix.Trace.default_traceln)
        | Alloc ->
          Some (fun k -> 
            let k = { Suspended.k; tid } in
            alloc_buf st k)
        | Free buf ->
          Some (fun k -> 
            free_buf st buf;
            continue k ())
        | _ -> None
    }
  in
  let main_done = ref false in
  let `Exit_scheduler = fork ~tid:(Ctf.mint_id ()) (fun () ->
      Fun.protect (fun () -> main stdenv)
        ~finally:(fun () -> main_done := true)
  ) in
  if not !main_done then
    failwith "Deadlock detected: no events scheduled but main function hasn't returned";
  Log.debug (fun l -> l "exit")
