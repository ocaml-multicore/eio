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

(* SIGPIPE makes no sense in a modern application. *)
let () = Sys.(set_signal sigpipe Signal_ignore)

type amount = Exactly of int | Upto of int

module FD = struct
  type t = {
    mutable fd : [`Open of Unix.file_descr | `Closed]
  }

  let get op = function
    | { fd = `Open fd } -> fd
    | { fd = `Closed } -> invalid_arg (op ^ ": file descriptor used after calling close!")

  let of_unix fd = { fd = `Open fd }
  let to_unix = get "to_unix"

  let is_open = function
    | { fd = `Open _ } -> true
    | { fd = `Closed } -> false

  let close t =
    let fd = get "close" t in
    t.fd <- `Closed;
    Unix.close fd
end

type rw_req = {
  op: [`R|`W];
  file_offset: int option;
  fd: FD.t;
  len: amount;
  buf: Uring.Region.chunk;
  mutable cur_off: int;
  action: (int, [`Exit_scheduler]) continuation;
}

(* Type of user-data attached to jobs. *)
type io_job =
  | Noop
  | Read : rw_req -> io_job
  | Poll_add : (int, [`Exit_scheduler]) continuation -> io_job
  | Write : rw_req -> io_job

type runnable =
  | Thread : ('a, [`Exit_scheduler]) continuation * 'a -> runnable
  | Failed_thread : ('a, [`Exit_scheduler]) continuation * exn -> runnable

type t = {
  uring: io_job Uring.t;
  mem: Uring.Region.t;
  io_q: (t -> unit) Queue.t;     (* waiting for room on [uring] *)
  mem_q : (Uring.Region.chunk, [`Exit_scheduler]) continuation Queue.t;
  run_q : runnable Queue.t;
  sleep_q: Zzz.t;
  mutable io_jobs: int;
}

let rec submit_rw_req st ({op; file_offset; fd; buf; len; cur_off; _} as req) =
  let fd = FD.get "submit_rw_req" fd in
  let {uring;io_q;_} = st in
  let off = Uring.Region.to_offset buf + cur_off in
  let len = match len with Exactly l | Upto l -> l in
  let len = len - cur_off in
  let subm =
    match op with
    |`R -> Uring.read uring ?file_offset fd off len (Read req)
    |`W -> Uring.write uring ?file_offset fd off len (Write req)
  in
  if not subm then (* wait until an sqe is available *)
    Queue.push (fun st -> submit_rw_req st req) io_q

(* TODO bind from unixsupport *)
let errno_is_retry = function -62 | -11 | -4 -> true |_ -> false

let enqueue_read st action (file_offset,fd,buf,len) =
  let req = { op=`R; file_offset; len; fd; cur_off = 0; buf; action} in
  Log.debug (fun l -> l "read: submitting call");
  submit_rw_req st req

let rec enqueue_poll_add st action fd poll_mask =
  Logs.debug (fun l -> l "poll_add: submitting call");
  let subm = Uring.poll_add st.uring (FD.get "poll_add" fd) poll_mask (Poll_add action) in
  if not subm then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_poll_add st action fd poll_mask) st.io_q

let enqueue_write st action (file_offset,fd,buf,len) =
  let req = { op=`W; file_offset; len; fd; cur_off = 0; buf; action} in
  Log.debug (fun l -> l "write: submitting call");
  submit_rw_req st req

let submit_pending_io st =
  match Queue.take_opt st.io_q with
  | None -> ()
  | Some fn -> fn st

(* Switch control to the next ready continuation.
   If none is ready, wait until we get an event to wake one and then switch.
   Returns only if there is nothing to do and no queued operations. *)
let rec schedule ({run_q; sleep_q; mem_q; uring; _} as st) : [`Exit_scheduler] =
  (* This is not a fair scheduler *)
  (* Wakeup any paused fibres *)
  match Queue.take run_q with
  | Thread (k, v) -> continue k v               (* We already have a runnable task *)
  | Failed_thread (k, ex) -> discontinue k ex
  | exception Queue.Empty ->
    match Zzz.restart_threads sleep_q with
    | Some k -> continue k ()                   (* A sleeping task is now due *)
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
      ) else match Uring.wait ?timeout uring with
        | None ->
          assert (timeout <> None);
          schedule st                                   (* Woken by a timeout, which is now due *)
        | Some (runnable, res) -> begin
            st.io_jobs <- st.io_jobs - 1;
            submit_pending_io st;                       (* If something was waiting for a slot, submit it now. *)
            match runnable with
            | Read req -> 
              Log.debug (fun l -> l "read returned");
              complete_rw_req st req res
            | Write req ->
              Log.debug (fun l -> l "write returned");
              complete_rw_req st req res
            | Poll_add k ->
              Logs.debug (fun l -> l "poll_add returned");
              continue k res
            | Noop -> assert false
          end
and complete_rw_req st ({len; cur_off; action; _} as req) res =
  match res, len with
  | 0, _ -> discontinue action End_of_file
  | e, _ when e < 0 ->
    if errno_is_retry e then (
      submit_rw_req st req;
      schedule st
    ) else (
      continue action e
    )
  | n, Exactly len when n < len - cur_off ->
    req.cur_off <- req.cur_off + n;
    submit_rw_req st req;
    schedule st
  | _, Exactly len -> continue action len
  | n, Upto _ -> continue action n

let enqueue_thread st k x =
  Queue.push (Thread (k, x)) st.run_q

let enqueue_failed_thread st k ex =
  Queue.push (Failed_thread (k, ex)) st.run_q

let alloc_buf st k =
  Log.debug (fun l -> l "alloc: %d" (Uring.Region.avail st.mem));
  match Uring.Region.alloc st.mem with
  | buf -> continue k buf 
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

effect Fork  : (unit -> 'a) -> 'a Promise.t
let fork f =
  perform (Fork f)

effect Fork_detach  : (unit -> unit) * (exn -> unit) -> unit
let fork_detach f ~on_error =
  perform (Fork_detach (f, on_error))

effect Yield : unit
let yield () =
  perform Yield

effect ERead : (int option * FD.t * Uring.Region.chunk * amount) -> int

let read_exactly ?file_offset fd buf len =
  let res = perform (ERead (file_offset, fd, buf, Exactly len)) in
  Log.debug (fun l -> l "read_exactly: woken up after read");
  if res < 0 then
    raise (Unix.Unix_error (Uring.error_of_errno res, "read_exactly", ""))

let read_upto ?file_offset fd buf len =
  let res = perform (ERead (file_offset, fd, buf, Upto len)) in
  Log.debug (fun l -> l "read_upto: woken up after read");
  if res < 0 then
    raise (Unix.Unix_error (Uring.error_of_errno res, "read_upto", ""))
  else
    res

effect EPoll_add : FD.t * Uring.Poll_mask.t -> int

let await_readable fd =
  let res = perform (EPoll_add (fd, Uring.Poll_mask.(pollin + pollerr))) in
  Logs.debug (fun l -> l "await_readable: woken up");
  if res < 0 then
    raise (Unix.Unix_error (Uring.error_of_errno res, "await_readable", ""))

let await_writable fd =
  let res = perform (EPoll_add (fd, Uring.Poll_mask.(pollout + pollerr))) in
  Logs.debug (fun l -> l "await_writable: woken up");
  if res < 0 then
    raise (Unix.Unix_error (Uring.error_of_errno res, "await_writable", ""))

effect EWrite : (int option * FD.t * Uring.Region.chunk * amount) -> int

let write ?file_offset fd buf len =
  let res = perform (EWrite (file_offset, fd, buf, Exactly len)) in
  Log.debug (fun l -> l "write: woken up after write");
  if res < 0 then
    raise (Unix.Unix_error (Uring.error_of_errno res, "write", ""))

effect Alloc : Uring.Region.chunk
let alloc () = perform Alloc

effect Free : Uring.Region.chunk -> unit
let free buf = perform (Free buf)

let openfile path flags mode =
  FD.of_unix (Unix.openfile path flags mode)

let fstat fd =
  Unix.fstat (FD.get "fstat" fd)

let shutdown socket command =
  Unix.shutdown (FD.get "shutdown" socket) command

let accept socket =
  await_readable socket;
  let conn, addr = Unix.accept ~cloexec:true (FD.get "accept" socket) in
  FD.of_unix conn, addr

let run ?(queue_depth=64) ?(block_size=4096) main =
  Log.debug (fun l -> l "starting run");
  (* TODO unify this allocation API around baregion/uring *)
  let fixed_buf_len = block_size * queue_depth in
  let uring = Uring.create ~fixed_buf_len ~queue_depth ~default:Noop () in
  let buf = Uring.buf uring in 
  let mem = Uring.Region.init ~block_size buf queue_depth in
  let run_q = Queue.create () in
  let sleep_q = Zzz.init () in
  let io_q = Queue.create () in
  let mem_q = Queue.create () in
  let st = { mem; uring; run_q; io_q; mem_q; sleep_q; io_jobs = 0 } in
  Log.debug (fun l -> l "starting main thread");
  let rec fork fn =
    match fn () with
    | () -> schedule st
    | effect (ERead args) k ->
      enqueue_read st k args;
      schedule st
    | effect (EPoll_add (fd, poll_mask)) k ->
      enqueue_poll_add st k fd poll_mask;
      schedule st
    | effect (EWrite args) k ->
      enqueue_write st k args;
      schedule st
    | effect Yield k ->
      enqueue_thread st k ();
      schedule st
    | effect (Sleep d) k ->
      Zzz.sleep sleep_q d k;
      schedule st
    | effect (Promise.Await q) k ->
      let when_resolved = function
        | Ok v -> enqueue_thread st k v
        | Error ex -> enqueue_failed_thread st k ex
      in
      Promise.add_waiter q when_resolved;
      schedule st
    | effect (Fork f) k ->
      let promise, resolver = Promise.create () in
      enqueue_thread st k promise;
      fork (fun () ->
          match f () with
          | x -> Promise.fulfill resolver x
          | exception ex ->
            Log.debug (fun f -> f "Forked fibre failed: %a" Fmt.exn ex);
            Promise.break resolver ex
        )
    | effect (Fork_detach (f, on_error)) k ->
      enqueue_thread st k ();
      fork (fun () -> try f () with ex -> on_error ex)
    | effect Alloc k ->
      alloc_buf st k
    | effect (Free buf) k ->
      free_buf st buf;
      continue k ()
  in
  let `Exit_scheduler = fork main in
  Log.debug (fun l -> l "exit")
