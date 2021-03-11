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

type rw_req = {
  op: [`R|`W];
  file_offset: int option;
  fd: Unix.file_descr;
  len: int;
  buf: Uring.Region.chunk;
  mutable cur_off: int;
  action: (int, unit) continuation;
}

type io_job =
| Noop
| Read : rw_req -> io_job
| Write : rw_req -> io_job

type runnable =
| Thread : ('a, unit) continuation * 'a -> runnable

type t = {
  uring: io_job Uring.t;
  mem: Uring.Region.t;
  io_q: rw_req Queue.t;     (* waiting for room on [uring] *)
  mem_q : (Uring.Region.chunk, unit) continuation Queue.t;
  run_q : runnable Queue.t;
  sleep_q: Zzz.t;
  mutable io_jobs: int;
}

let submit_rw_req {uring;io_q;_} ({op; file_offset; fd; buf; len; cur_off; _} as req) =
  let off = Uring.Region.to_offset buf + cur_off in
  let len = len - cur_off in
  let subm =
    match op with
    |`R -> Uring.read uring ?file_offset fd off len (Read req)
    |`W -> Uring.write uring ?file_offset fd off len (Write req)
  in
  if not subm then (* wait until an sqe is available *)
    Queue.push req io_q


(* TODO bind from unixsupport *)
let errno_is_retry = function -62 | -11 | -4 -> true |_ -> false

let complete_rw_req st ({len; cur_off; action; _} as req) res =
  match res with
  | 0 -> discontinue action (Failure "end of file") (* TODO expose EOF exception *)
  | n when errno_is_retry n ->
     submit_rw_req st req
  | n when n < len - cur_off ->
     req.cur_off <- req.cur_off + n;
     submit_rw_req st req
  | _ -> continue action len

let enqueue_read st action (file_offset,fd,buf,len) =
  let req = { op=`R; file_offset; len; fd; cur_off = 0; buf; action} in
  Logs.debug (fun l -> l "read: submitting call");
  submit_rw_req st req

let enqueue_write st action (file_offset,fd,buf,len) =
  let req = { op=`W; file_offset; len; fd; cur_off = 0; buf; action} in
  Logs.debug (fun l -> l "write: submitting call");
  submit_rw_req st req
 
let rec wakeup_paused run_q =
  match Queue.take run_q with
  | Thread (k, v) ->
      continue k v;
      wakeup_paused run_q
  | exception Queue.Empty -> ()

let submit_pending_io st =
  match Queue.take_opt st.io_q with
  | None -> ()
  | Some req -> submit_rw_req st req

let rec schedule ({run_q; sleep_q; mem_q; uring; _} as st) =
  (* This is not a fair scheduler *)
  (* Wakeup any paused fibres *)
  wakeup_paused run_q;
  Zzz.restart_threads sleep_q;
  let num_jobs = Uring.submit uring in
  st.io_jobs <- st.io_jobs + num_jobs;
  let timeout = Zzz.select_next sleep_q in
  Logs.debug (fun l -> l "scheduler: %d sub / %d total, timeout %s" num_jobs st.io_jobs
    (match timeout with None -> "inf" | Some v -> string_of_float v));
  if Queue.length run_q = 0 && Queue.length mem_q = 0 && timeout = None && st.io_jobs = 0 then begin
    Logs.debug (fun l -> l "schedule: exiting");
  end else match Uring.wait ?timeout uring with
  | None -> 
     Logs.debug (fun l -> l "wait returned none");
     schedule st (* TODO this is a bad situation to be in, likely fatal *)
  | Some (runnable, res) -> begin
     st.io_jobs <- st.io_jobs - 1;
     submit_pending_io st;
     match runnable with
     | Read req -> 
        Logs.debug (fun l -> l "read returned");
        complete_rw_req st req res
     | Write req ->
        Logs.debug (fun l -> l "write returned");
        complete_rw_req st req res
     | Noop -> ()
  end

let enqueue_thread st k x =
  Queue.push (Thread (k, x)) st.run_q

let alloc_buf st k =
  Logs.debug (fun l -> l "alloc: %d" (Uring.Region.avail st.mem));
  match Uring.Region.alloc st.mem with
  | buf -> continue k buf 
  | exception Uring.Region.No_space -> Queue.push k st.mem_q

let free_buf st buf =
  match Queue.take_opt st.mem_q with
  | None -> Uring.Region.free buf
  | Some k -> enqueue_thread st k buf

effect Sleep : float -> unit
let sleep d =
  perform (Sleep d)

effect Fork  : (unit -> unit) -> unit
let fork f =
  perform (Fork f)

effect Yield : unit
let yield () =
  perform Yield

effect ERead : (int option * Unix.file_descr * Uring.Region.chunk * int) -> int
 
let read ?file_offset fd buf len =
  let res = perform (ERead (file_offset, fd, buf, len)) in
  Logs.debug (fun l -> l "read: woken up after read");
  if res < 0 then
    raise (Failure (Fmt.strf "read %d" res)) (* FIXME Unix_error *)

effect EWrite : (int option * Unix.file_descr * Uring.Region.chunk * int) -> int

let write ?file_offset fd buf len =
  let res = perform (EWrite (file_offset, fd, buf, len)) in
  Logs.debug (fun l -> l "write: woken up after read");
  if res < 0 then
    raise (Failure (Fmt.strf "write %d" res)) (* FIXME Unix_error *)

effect Alloc : Uring.Region.chunk
let alloc () = perform Alloc

effect Free : Uring.Region.chunk -> unit
let free buf = perform (Free buf)

let run ?(queue_depth=64) ?(block_size=4096) main =
  Logs.debug (fun l -> l "starting run");
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
  Logs.debug (fun l -> l "starting main thread");
  let rec fork fn =
    match fn () with
    | () -> schedule st
    | exception exn ->
       Logs.err (fun l -> l "exn: %a" Fmt.exn exn);
       schedule st
    | effect (ERead args) k ->
       enqueue_read st k args;
       schedule st
    | effect (EWrite args) k ->
       enqueue_write st k args;
       schedule st
    | effect Yield k ->
       enqueue_thread st k ();
       schedule st
    | effect (Sleep d) k ->
       Zzz.sleep sleep_q d (Some k);
       schedule st
    | effect (Fork f) k ->
       enqueue_thread st k ();
       fork f
    | effect Alloc k ->
       alloc_buf st k
    | effect (Free buf) k ->
       free_buf st buf;
       continue k ()
   in
   fork main;
   Logs.debug (fun l -> l "exit")
