(*
 * Copyright (C) 2021 Thomas Leonard
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

let src = Logs.Src.create "eio_luv" ~doc:"Eio backend using luv"
module Log = (val Logs.src_log src : Logs.LOG)

open Eio.Std

module Ctf = Eio.Private.Ctf

module Fiber_context = Eio.Private.Fiber_context
module Lf_queue = Eio_utils.Lf_queue

(* SIGPIPE makes no sense in a modern application. *)
let () = Sys.(set_signal sigpipe Signal_ignore)

exception Luv_error of Luv.Error.t

let () =
  Printexc.register_printer @@ function
  | Luv_error e -> Some (Printf.sprintf "Eio_luv.Luv_error(%s) (* %s *)" (Luv.Error.err_name e) (Luv.Error.strerror e))
  | _ -> None

let wrap_error ~path e =
  let ex = Luv_error e in
  match e with
  | `EEXIST -> Eio.Fs.Already_exists (path, ex)
  | `ENOENT -> Eio.Fs.Not_found (path, ex)
  | _ -> ex

let wrap_flow_error e =
  let ex = Luv_error e in
  match e with
  | `ECONNRESET
  | `EPIPE -> Eio.Net.Connection_reset ex
  | _ -> ex

let or_raise = function
  | Ok x -> x
  | Error e -> raise (Luv_error e)

let or_raise_path path = function
  | Ok x -> x
  | Error e -> raise (wrap_error ~path e)

(* Luv can't handle buffers with more than 2^32-1 bytes, limit it to
   31bit so we can also make sure 32bit archs don't overflow.
   See https://github.com/ocaml-multicore/eio/issues/335 *)
let max_luv_buffer_size = 0x7fffffff

(* Return as much of [buf] as luv can handle. This is suitable if a short read/write is acceptable. *)
let cstruct_to_luv_truncate buf =
  Cstruct.to_bigarray @@
  if Cstruct.length buf <= max_luv_buffer_size then buf
  else Cstruct.sub buf 0 max_luv_buffer_size

(* Raise if the buffer is too big. Use this for atomic reads and writes. *)
let cstruct_to_luv_exn buf =
  if Cstruct.length buf <= max_luv_buffer_size then Cstruct.to_bigarray buf
  else Fmt.invalid_arg "Buffer too large for luv (%d > %d)" (Cstruct.length buf) max_luv_buffer_size

(* For vectors, we can just split long buffers into two. *)
let rec cstructv_to_luv = function
  | [] -> []
  | x :: xs when Cstruct.length x <= max_luv_buffer_size ->
    Cstruct.to_bigarray x :: cstructv_to_luv xs
  | x :: xs ->
    let x1, x2 = Cstruct.split x max_luv_buffer_size in
    Cstruct.to_bigarray x1 :: cstructv_to_luv (x2 :: xs)

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

  let continue_result t = function
    | Ok x -> continue t x
    | Error x -> discontinue t x
end

type runnable =
  | IO
  | Thread of (unit -> unit)

type fd_event_waiters = {
  fd : Unix.file_descr;
  handle : Luv.Poll.t;
  read : unit Suspended.t Lwt_dllist.t;
  write : unit Suspended.t Lwt_dllist.t;
}

module Fd_map = Map.Make(struct type t = Unix.file_descr let compare = Stdlib.compare end)

type t = {
  loop : Luv.Loop.t;
  async : Luv.Async.t;                          (* Will process [run_q] when prodded. *)
  run_q : runnable Lf_queue.t;
  mutable fd_map : fd_event_waiters Fd_map.t;   (* Used for mapping readable/writable poll handles *)
}

type _ Effect.t += Await : (Luv.Loop.t -> Eio.Private.Fiber_context.t -> ('a -> unit) -> unit) -> 'a Effect.t

type _ Effect.t += Enter : (t -> 'a Suspended.t -> unit) -> 'a Effect.t
type _ Effect.t += Enter_unchecked : (t -> 'a Suspended.t -> unit) -> 'a Effect.t

let enter fn = Effect.perform (Enter fn)
let enter_unchecked fn = Effect.perform (Enter_unchecked fn)

let enqueue_thread t k v =
  Lf_queue.push t.run_q (Thread (fun () -> Suspended.continue k v));
  Luv.Async.send t.async |> or_raise

let enqueue_result_thread t k r =
  Lf_queue.push t.run_q (Thread (fun () -> Suspended.continue_result k r));
  Luv.Async.send t.async |> or_raise

let enqueue_failed_thread t k ex =
  Lf_queue.push t.run_q (Thread (fun () -> Suspended.discontinue k ex));
  Luv.Async.send t.async |> or_raise

module Poll : sig
  val await_readable : t -> unit Suspended.t -> Unix.file_descr -> unit
  val await_writable : t -> unit Suspended.t -> Unix.file_descr -> unit

  val cancel_all : t -> Unix.file_descr -> unit
  (** [cancel_all t fd] should be called just before [fd] is closed.
      Any waiters will be cancelled. *)
end = struct
  (* According to the libuv docs:
     - It is not okay to have multiple poll handles for the same file descriptor.
     - The user should not close the file descriptor while it is being polled
        by an active poll handle.

     As such, we keep track of the mapping between poll handle and FD in the [fd_map].
     This contains two queues of waiters for a given handle; those waiting for readability and
     those waiting for writability.

     Whenever the [read] queue is non-empty we enable polling for the READ event, and
     whenevent the [write] queue is non-empty we enable polling for WRTIE. When both are
     empty we stop polling. *)

  let apply_all q fn =
    let rec loop = function
      | None -> ()
      | Some v -> fn v; loop (Lwt_dllist.take_opt_r q)
    in
    loop (Lwt_dllist.take_opt_r q)

  let enqueue_and_remove t fn (k : unit Suspended.t) v =
    if Fiber_context.clear_cancel_fn k.fiber then
      fn t k v

  let rec poll_callback t events r =
    begin match r with
      | Ok (es : Luv.Poll.Event.t list) ->
        if List.mem `READABLE es then apply_all events.read (fun k -> enqueue_and_remove t enqueue_thread k ());
        if List.mem `WRITABLE es then apply_all events.write (fun k -> enqueue_and_remove t enqueue_thread k ());
      | Error e ->
        apply_all events.read (fun k -> enqueue_and_remove t enqueue_failed_thread k (Luv_error e));
        apply_all events.write (fun k -> enqueue_and_remove t enqueue_failed_thread k (Luv_error e))
    end;
    update t events
  and update t events =
    let m = if Lwt_dllist.is_empty events.write then [] else [`WRITABLE] in
    let m = if Lwt_dllist.is_empty events.read then m else `READABLE :: m in
    if m = [] then (
      Luv.Poll.stop events.handle |> or_raise;
      t.fd_map <- Fd_map.remove events.fd t.fd_map
    ) else (
      Luv.Poll.start events.handle m (poll_callback t events)
    )

  let cancel_all t fd =
    match Fd_map.find_opt fd t.fd_map with
    | Some v ->
      let ex = Failure "Closed file descriptor whilst polling" in
      apply_all v.read (fun k -> enqueue_and_remove t enqueue_failed_thread k ex);
      apply_all v.write (fun k -> enqueue_and_remove t enqueue_failed_thread k ex);
      update t v
    | None -> ()

  let get_events t fd =
    match Fd_map.find_opt fd t.fd_map with
    | Some events -> events
    | None ->
      let handle = Luv.Poll.init ~loop:t.loop (Obj.magic fd : int) |> or_raise in
      let events = {
        fd;
        handle;
        read = Lwt_dllist.create ();
        write = Lwt_dllist.create ();
      } in
      t.fd_map <- Fd_map.add fd events t.fd_map;
      events

  let await t (k:unit Suspended.t) events queue =
    let was_empty = Lwt_dllist.is_empty queue in
    let node = Lwt_dllist.add_l k queue in
    (* Set the fiber cancel function, which first removes the continutation
       from the list to continue when the FD becomes readable or writeable.
       Then it checks if the poll handle can be stopped and the mapping
       removed. *)
    Fiber_context.set_cancel_fn k.fiber (fun ex ->
        Lwt_dllist.remove node;
        if Lwt_dllist.is_empty queue then update t events;
        enqueue_failed_thread t k ex
      );
    if was_empty then update t events

  let await_readable t k fd =
    let events = get_events t fd in
    await t k events events.read

  let await_writable t k fd =
    let events = get_events t fd in
    await t k events events.write
end

(* Can only be called from our domain. *)
let enqueue_at_head t k v =
  Lf_queue.push_head t.run_q (Thread (fun () -> Suspended.continue k v));
  Luv.Async.send t.async |> or_raise

let get_loop () =
  enter_unchecked @@ fun t k ->
  Suspended.continue k t.loop

let unix_fstat fd =
  let ust = Unix.LargeFile.fstat fd in
  let st_kind : Eio.File.Stat.kind =
    match ust.st_kind with
    | Unix.S_REG  -> `Regular_file
    | Unix.S_DIR  -> `Directory
    | Unix.S_CHR  -> `Character_special
    | Unix.S_BLK  -> `Block_device
    | Unix.S_LNK  -> `Symbolic_link
    | Unix.S_FIFO -> `Fifo
    | Unix.S_SOCK -> `Socket
  in
  Eio.File.Stat.{
    dev     = ust.st_dev   |> Int64.of_int;
    ino     = ust.st_ino   |> Int64.of_int;
    kind    = st_kind;
    perm    = ust.st_perm;
    nlink   = ust.st_nlink |> Int64.of_int;
    uid     = ust.st_uid   |> Int64.of_int;
    gid     = ust.st_gid   |> Int64.of_int;
    rdev    = ust.st_rdev  |> Int64.of_int;
    size    = ust.st_size  |> Optint.Int63.of_int64;
    atime   = ust.st_atime;
    mtime   = ust.st_mtime;
    ctime   = ust.st_ctime;
  }

module Low_level = struct
  type 'a or_error = ('a, Luv.Error.t) result

  exception Luv_error = Luv_error
  let or_raise = or_raise

  let await fn =
    Effect.perform (Await fn)

  let await_exn fn =
    Effect.perform (Await fn) |> or_raise

  let await_with_cancel ~request fn =
    enter (fun st k ->
        let cancel_reason = ref None in
        Eio.Private.Fiber_context.set_cancel_fn k.fiber (fun ex ->
            cancel_reason := Some ex;
            match Luv.Request.cancel request with
            | Ok () -> ()
            | Error e -> Log.debug (fun f -> f "Cancel failed: %s" (Luv.Error.strerror e))
          );
        fn st.loop (fun v ->
            if Eio.Private.Fiber_context.clear_cancel_fn k.fiber then (
              enqueue_thread st k v
            ) else (
              (* Cancellations always come from the same domain, so we can be sure
                 that [cancel_reason] is set by now. *)
              enqueue_failed_thread st k (Option.get !cancel_reason)
            )
          )
      )

  module Handle = struct
    type 'a t = {
      mutable release_hook : Eio.Switch.hook;        (* Use this on close to remove switch's [on_release] hook. *)
      close_unix : bool;
      mutable fd : [`Open of 'a Luv.Handle.t | `Closed]
    } constraint 'a = [< `Poll | `Stream of [< `Pipe | `TCP | `TTY ] | `UDP ]

    let get op = function
      | { fd = `Open fd; _ } -> fd
      | { fd = `Closed ; _ } -> invalid_arg (op ^ ": handle used after calling close!")

    let is_open = function
      | { fd = `Open _; _ } -> true
      | { fd = `Closed; _ } -> false

    let close t =
      Ctf.label "close";
      let fd = get "close" t in
      t.fd <- `Closed;
      Eio.Switch.remove_hook t.release_hook;
      if t.close_unix then (
        enter_unchecked @@ fun t k ->
        begin match Luv.Handle.fileno fd with
          | Ok fd -> Poll.cancel_all t (Luv_unix.Os_fd.Fd.to_unix fd)
          | Error `EBADF -> ()  (* We don't have a Unix FD yet, so we can't be watching it. *)
          | Error e -> raise (Luv_error e)
        end;
        Luv.Handle.close fd (enqueue_thread t k)
      )

    let ensure_closed t =
      if is_open t then close t

    let to_luv x = get "to_luv" x

    let of_luv_no_hook ~close_unix fd =
      { fd = `Open fd; release_hook = Eio.Switch.null_hook; close_unix }

    let of_luv ?(close_unix=true) ~sw fd =
      let t = of_luv_no_hook ~close_unix fd in
      t.release_hook <- Switch.on_release_cancellable sw (fun () -> ensure_closed t);
      t

    let to_unix_opt op (t:_ t) =
      match Luv.Handle.fileno (to_luv t) with
      | Error _ -> None
      | Ok os_fd ->
        let fd = Luv_unix.Os_fd.Fd.to_unix os_fd in
        match op with
        | `Peek -> Some fd
        | `Take ->
          t.fd <- `Closed;
          Eio.Switch.remove_hook t.release_hook;
          Some fd
  end

  module File = struct
    type t = {
      mutable release_hook : Eio.Switch.hook;        (* Use this on close to remove switch's [on_release] hook. *)
      close_unix : bool;
      mutable fd : [`Open of Luv.File.t | `Closed]
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
      Eio.Switch.remove_hook t.release_hook;
      enter_unchecked (fun st k ->
          let os_fd = Luv.File.get_osfhandle fd |> or_raise in
          let unix_fd = Luv_unix.Os_fd.Fd.to_unix os_fd in
          Poll.cancel_all st unix_fd;
          Luv.File.close ~loop:st.loop fd (enqueue_thread st k)
        ) |> or_raise

    let ensure_closed t =
      if is_open t then close t

    let to_luv = get "to_luv"

    let of_luv_no_hook ~close_unix fd =
      { fd = `Open fd; release_hook = Eio.Switch.null_hook; close_unix }

    let of_luv ?(close_unix=true) ~sw fd =
      let t = of_luv_no_hook ~close_unix fd in
      t.release_hook <- Switch.on_release_cancellable sw (fun () -> ensure_closed t);
      t

    let fstat fd =
      let request = Luv.File.Request.make () in
      await_with_cancel ~request (fun loop -> Luv.File.fstat ~loop ~request (to_luv fd))

    let open_ ~sw ?mode path flags =
      let request = Luv.File.Request.make () in
      await_with_cancel ~request (fun loop -> Luv.File.open_ ~loop ?mode ~request path flags)
      |> Result.map (of_luv ~sw)

    let read ?file_offset fd bufs =
      let request = Luv.File.Request.make () in
      await_with_cancel ~request (fun loop -> Luv.File.read ~loop ~request ?file_offset (get "read" fd) bufs)

    let write_single ?file_offset fd bufs =
      let request = Luv.File.Request.make () in
      await_with_cancel ~request (fun loop -> Luv.File.write ~loop ~request ?file_offset (get "write" fd) bufs)

    let rec write fd bufs =
      match write_single fd bufs with
      | Error _ as e -> e
      | Ok sent ->
        let rec aux = function
          | [] -> Ok ()
          | x :: xs when Luv.Buffer.size x = 0 -> aux xs
          | bufs -> write fd bufs
        in
        aux @@ Luv.Buffer.drop bufs (Unsigned.Size_t.to_int sent)

    let realpath path =
      let request = Luv.File.Request.make () in
      await_with_cancel ~request (fun loop -> Luv.File.realpath ~loop ~request path)

    let mkdir ~mode path =
      let request = Luv.File.Request.make () in
      await_with_cancel ~request (fun loop -> Luv.File.mkdir ~loop ~request ~mode path)

    let unlink path =
      let request = Luv.File.Request.make () in
      await_with_cancel ~request (fun loop -> Luv.File.unlink ~loop ~request path)

    let rmdir path =
      let request = Luv.File.Request.make () in
      await_with_cancel ~request (fun loop -> Luv.File.rmdir ~loop ~request path)

    let rename old_path new_path =
      let request = Luv.File.Request.make () in
      await_with_cancel ~request (fun loop -> Luv.File.rename ~loop ~request old_path ~to_:new_path)

    let opendir path =
      let request = Luv.File.Request.make () in
      await_with_cancel ~request (fun loop -> Luv.File.opendir ~loop ~request path)

    let closedir path =
      let request = Luv.File.Request.make () in
      await_with_cancel ~request (fun loop -> Luv.File.closedir ~loop ~request path)

    let with_dir_to_read path fn =
      match opendir path with
      | Ok dir ->
        Fun.protect ~finally:(fun () -> closedir dir |> or_raise) @@ fun () -> fn dir
      | Error _ as e -> e

    let readdir path =
      let fn dir =
        let request = Luv.File.Request.make () in
        match await_with_cancel ~request (fun loop -> Luv.File.readdir ~loop ~request dir) with
        | Ok dirents ->
          let dirs = Array.map (fun v -> v.Luv.File.Dirent.name) dirents |> Array.to_list in
          Ok dirs
        | Error _ as e -> e
      in
        with_dir_to_read path fn

    let to_unix op t =
      let os_fd = Luv.File.get_osfhandle (get "to_unix" t) |> or_raise in
      let fd = Luv_unix.Os_fd.Fd.to_unix os_fd in
      match op with
      | `Peek -> fd
      | `Take ->
        t.fd <- `Closed;
        Eio.Switch.remove_hook t.release_hook;
        fd
  end

  module Random = struct
    let rec fill buf =
      let request = Luv.Random.Request.make () in
      match await_with_cancel ~request (fun loop -> Luv.Random.random ~loop ~request buf) with
      | Ok x -> x
      | Error `EINTR -> fill buf
      | Error x -> raise (Luv_error x)
  end

  module Stream = struct
    type 'a t = [`Stream of 'a] Handle.t

    let rec read_into (sock:'a t) buf =
      let r = enter (fun t k ->
          Fiber_context.set_cancel_fn k.fiber (fun ex ->
              Luv.Stream.read_stop (Handle.get "read_into:cancel" sock) |> or_raise;
              enqueue_failed_thread t k ex
            );
          Luv.Stream.read_start (Handle.get "read_start" sock) ~allocate:(fun _ -> buf) (fun r ->
              Luv.Stream.read_stop (Handle.get "read_stop" sock) |> or_raise;
              if Fiber_context.clear_cancel_fn k.fiber then enqueue_thread t k r
            )
        ) in
      match r with
      | Ok buf' ->
        let len = Luv.Buffer.size buf' in
        if len > 0 then len
        else read_into sock buf       (* Luv uses a zero-length read to mean EINTR! *)
      | Error `EOF -> raise End_of_file
      | Error x -> raise (wrap_flow_error x)

    let rec skip_empty = function
      | empty :: xs when Luv.Buffer.size empty = 0 -> skip_empty xs
      | xs -> xs

    let rec write t bufs =
      let err, n =
        (* note: libuv doesn't seem to allow cancelling stream writes *)
        enter (fun st k ->
            Luv.Stream.write (Handle.get "write_stream" t) bufs @@ fun err n ->
            enqueue_thread st k (err, n)
          )
      in
      match err with
      | Error e -> raise (wrap_flow_error e)
      | Ok () ->
        match Luv.Buffer.drop bufs n |> skip_empty with
        | [] -> ()
        | bufs -> write t bufs

    let to_unix_opt = Handle.to_unix_opt

    let of_unix fd =
      Luv_unix.Os_fd.Socket.from_unix fd |> or_raise

    let connect_pipe ~sw path =
      let sock = Luv.Pipe.init ~loop:(get_loop ()) () |> or_raise |> Handle.of_luv ~sw in
      match await (fun _loop _fiber -> Luv.Pipe.connect (Handle.get "connect" sock) path) with
      | Ok () -> sock
      | Error e -> raise (Eio.Net.Connection_failure (Luv_error e))

    let connect_tcp ~sw addr =
      let sock = Luv.TCP.init ~loop:(get_loop ()) () |> or_raise in
      enter (fun st k ->
          Luv.TCP.connect sock addr (fun v ->
              ignore (Fiber_context.clear_cancel_fn k.fiber : bool);
              match v with
              | Ok () -> enqueue_thread st k ()
              | Error e ->
                Luv.Handle.close sock ignore;
                match Fiber_context.get_error k.fiber with
                | Some ex -> enqueue_failed_thread st k ex
                | None -> enqueue_failed_thread st k (Eio.Net.Connection_failure (Luv_error e))
            );
          Fiber_context.set_cancel_fn k.fiber (fun _ex ->
              match Luv.Handle.fileno sock with
              | Error _ -> ()
              | Ok os_fd ->
                let fd = Luv_unix.Os_fd.Fd.to_unix os_fd in
                Unix.shutdown fd Unix.SHUTDOWN_ALL;
                (* Luv.Handle.close sock ignore *)
            )
        );
      Handle.of_luv ~sw sock
  end

  module Poll = Poll

  let sleep_ms delay =
    enter @@ fun st k ->
    let timer = Luv.Timer.init ~loop:st.loop () |> or_raise in
    Fiber_context.set_cancel_fn k.fiber (fun ex ->
        Luv.Timer.stop timer |> or_raise;
        Luv.Handle.close timer (fun () -> ());
        enqueue_failed_thread st k ex
      );
    Luv.Timer.start timer delay (fun () ->
        if Fiber_context.clear_cancel_fn k.fiber then enqueue_thread st k ()
      ) |> or_raise

  let sleep_until due =
    let delay = 1000. *. (due -. Unix.gettimeofday ()) |> ceil |> truncate |> max 0 in
    sleep_ms delay

  (* https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml *)
  let getaddrinfo ~service node =
    let ( let* ) o f = Option.bind o f in
    let to_eio_sockaddr_t {Luv.DNS.Addr_info.family; addr; socktype; protocol; _ } =
      match family, socktype with
      | (`INET | `INET6),
        (`STREAM | `DGRAM) -> (
          let* host = Luv.Sockaddr.to_string addr in
          let* port = Luv.Sockaddr.port addr in
          let ipaddr = Unix.inet_addr_of_string host |> Eio_unix.Ipaddr.of_unix in
          match protocol with
          | 6 -> Some (`Tcp (ipaddr, port))
          | 17 -> Some (`Udp (ipaddr, port))
          | _ -> None)
      | _ -> None
    in
    let request = Luv.DNS.Addr_info.Request.make () in
    await_with_cancel ~request (fun loop -> Luv.DNS.getaddrinfo ~loop ~request ~service ~node ())
    |> or_raise
    |> List.filter_map to_eio_sockaddr_t

end

open Low_level

type _ Eio.Generic.ty += FD : Low_level.File.t Eio.Generic.ty

type has_fd = < fd : Low_level.File.t >
type source = < Eio.Flow.source; Eio.Flow.close; has_fd >
type sink   = < Eio.Flow.sink  ; Eio.Flow.close; has_fd >

let get_fd (t : <has_fd; ..>) = t#fd

let get_fd_opt t = Eio.Generic.probe t FD

let flow fd = object (_ : <source; sink; ..>)
  method fd = fd
  method close = Low_level.File.close fd
  method unix_fd op = File.to_unix op fd

  method stat = unix_fstat (File.to_unix `Peek fd)

  method probe : type a. a Eio.Generic.ty -> a option = function
    | FD -> Some fd
    | Eio_unix.Private.Unix_file_descr op -> Some (File.to_unix op fd)
    | _ -> None

  method read_into buf =
    let buf = cstruct_to_luv_truncate buf in
    match File.read fd [buf] |> or_raise |> Unsigned.Size_t.to_int with
    | 0 -> raise End_of_file
    | got -> got

  method pread ~file_offset bufs =
    let bufs = cstructv_to_luv bufs in
    let file_offset = Optint.Int63.to_int64 file_offset in
    match File.read ~file_offset fd bufs |> or_raise |> Unsigned.Size_t.to_int with
    | 0 -> raise End_of_file
    | got -> got

  method pwrite ~file_offset bufs =
    let bufs = cstructv_to_luv bufs in
    let file_offset = Optint.Int63.to_int64 file_offset in
    File.write_single ~file_offset fd bufs |> or_raise |> Unsigned.Size_t.to_int

  method read_methods = []

  method write bufs =
    let bufs = cstructv_to_luv bufs in
    File.write fd bufs |> or_raise

  method copy src =
    let buf = Luv.Buffer.create 4096 in
    try
      while true do
        let got = Eio.Flow.single_read src (Cstruct.of_bigarray buf) in
        let sub = Luv.Buffer.sub buf ~offset:0 ~length:got in
        File.write fd [sub] |> or_raise
      done
    with End_of_file -> ()
end

let source fd = (flow fd :> source)
let sink   fd = (flow fd :> sink)

let socket sock = object
  inherit Eio.Flow.two_way as super

  method! probe : type a. a Eio.Generic.ty -> a option = function
    | Eio_unix.Private.Unix_file_descr op -> Stream.to_unix_opt op sock
    | x -> super#probe x

  method unix_fd op = Stream.to_unix_opt op sock |> Option.get

  method read_into buf =
    let buf = cstruct_to_luv_truncate buf in
    Stream.read_into sock buf

  method! write bufs =
    let bufs = cstructv_to_luv bufs in
    Stream.write sock bufs

  method copy src =
    let buf = Luv.Buffer.create 4096 in
    try
      while true do
        let got = Eio.Flow.single_read src (Cstruct.of_bigarray buf) in
        let buf' = Luv.Buffer.sub buf ~offset:0 ~length:got in
        Stream.write sock [buf']
      done
    with End_of_file -> ()

  method close =
    Handle.close sock

  method shutdown = function
    | `Send -> await_exn (fun _loop _fiber -> Luv.Stream.shutdown (Handle.get "shutdown" sock))
    | `Receive | `All as cmd ->
      let fd = Stream.to_unix_opt `Peek sock |> Option.get in
      Unix.shutdown fd @@ match cmd  with
      | `Receive -> Unix.SHUTDOWN_RECEIVE
      | `All -> Unix.SHUTDOWN_ALL
end

class virtual ['a] listening_socket ~backlog sock = object (self)
  inherit Eio.Net.listening_socket as super

  method! probe : type a. a Eio.Generic.ty -> a option = function
    | Eio_unix.Private.Unix_file_descr op -> Stream.to_unix_opt op sock
    | x -> super#probe x

  val ready = Eio.Semaphore.make 0

  method private virtual make_client : 'a Luv.Stream.t
  method private virtual get_client_addr : 'a Stream.t -> Eio.Net.Sockaddr.stream

  method close = Handle.close sock

  method accept ~sw =
    Eio.Semaphore.acquire ready;
    let client = self#make_client |> Handle.of_luv_no_hook ~close_unix:true in
    match Luv.Stream.accept ~server:(Handle.get "accept" sock) ~client:(Handle.get "accept" client) with
    | Error e ->
      Handle.close client;
      raise (Luv_error e)
    | Ok () ->
      Switch.on_release sw (fun () -> Handle.ensure_closed client);
      let flow = (socket client :> <Eio.Flow.two_way; Eio.Flow.close>) in
      let client_addr = self#get_client_addr client in
      flow, client_addr

  initializer
    Luv.Stream.listen ~backlog (Handle.get "listen" sock) (fun x ->
        or_raise x;
        Eio.Semaphore.release ready
      )
end

(* TODO: implement, or maybe remove from the Eio API.
   Luv makes TCP sockets reuse_addr by default, and maybe that's fine everywhere.
   Extracting the FD will require https://github.com/aantron/luv/issues/120 *)
let luv_reuse_addr _sock _v = ()
let luv_reuse_port _sock _v = ()

let luv_addr_of_eio host port =
  let host = Unix.string_of_inet_addr (Eio_unix.Ipaddr.to_unix host) in
  match Luv.Sockaddr.ipv6 host port with
  | Ok addr -> addr
  | Error _ -> Luv.Sockaddr.ipv4 host port |> or_raise

let luv_ip_addr_to_eio addr =
  let host = Luv.Sockaddr.to_string addr |> Option.get in
  let port = Luv.Sockaddr.port addr |> Option.get in
  (Eio_unix.Ipaddr.of_unix (Unix.inet_addr_of_string host), port)

module Udp = struct
  type 'a t = [`UDP] Handle.t

  (* When the sender address in the callback of [recv_start] is [None], this usually indicates
     EAGAIN according to the luv documentation which can be ignored. Libuv calls the callback
     in case C programs wish to handle the allocated buffer in some way. *)
  let recv (sock:'a t) buf =
    let r = enter (fun t k ->
        Fiber_context.set_cancel_fn k.fiber (fun ex ->
            Luv.UDP.recv_stop (Handle.get "recv_into:cancel" sock) |> or_raise;
            enqueue_failed_thread t k ex
          );
        Luv.UDP.recv_start (Handle.get "recv_start" sock) ~allocate:(fun _ -> buf) (function
          | Ok (_, None, _) -> ()
          | Ok (buf, Some addr, flags) ->
            Luv.UDP.recv_stop (Handle.get "recv_stop" sock) |> or_raise;
            if Fiber_context.clear_cancel_fn k.fiber then enqueue_thread t k (Ok (buf, addr, flags))
          | Error _ as err ->
            Luv.UDP.recv_stop (Handle.get "recv_stop" sock) |> or_raise;
            if Fiber_context.clear_cancel_fn k.fiber then enqueue_thread t k err
          )
      ) in
    match r with
    | Ok (buf', sockaddr, _recv_flags) ->
      `Udp (luv_ip_addr_to_eio sockaddr), Luv.Buffer.size buf'
    | Error x -> raise (wrap_flow_error x)

  let send t buf = function
  | `Udp (host, port) ->
    let bufs = cstructv_to_luv [ buf ] in
    match await (fun _loop _fiber -> Luv.UDP.send (Handle.get "send" t) bufs (luv_addr_of_eio host port)) with
    | Ok () -> ()
    | Error e -> raise (wrap_flow_error e)
end

let udp_socket endp = object
  inherit Eio.Net.datagram_socket

  method close = Handle.close endp

  method send sockaddr bufs = Udp.send endp bufs sockaddr
  method recv buf =
    let buf = cstruct_to_luv_exn buf in
    Udp.recv endp buf
end

let listening_ip_socket ~backlog sock = object
  inherit [[ `TCP ]] listening_socket ~backlog sock

  method private make_client = Luv.TCP.init ~loop:(get_loop ()) () |> or_raise

  method private get_client_addr c =
    `Tcp (Luv.TCP.getpeername (Handle.get "get_client_addr" c) |> or_raise |> luv_ip_addr_to_eio)
end

let listening_unix_socket ~backlog sock = object
  inherit [[ `Pipe ]] listening_socket ~backlog sock

  method private make_client = Luv.Pipe.init ~loop:(get_loop ()) () |> or_raise
  method private get_client_addr c =
    `Unix (Luv.Pipe.getpeername (Handle.get "get_client_addr" c) |> or_raise)
end

let socket_domain_of = function
  | `UdpV4 -> `INET
  | `UdpV6 -> `INET6
  | `Udp (host, _)
  | `Tcp (host, _) ->
    Eio.Net.Ipaddr.fold host
      ~v4:(fun _ -> `INET)
      ~v6:(fun _ -> `INET6)

let net = object
  inherit Eio.Net.t

  method listen ~reuse_addr ~reuse_port ~backlog ~sw = function
    | `Tcp (host, port) ->
      let sock = Luv.TCP.init ~loop:(get_loop ()) () |> or_raise |> Handle.of_luv ~sw in
      luv_reuse_addr sock reuse_addr;
      luv_reuse_port sock reuse_port;
      let addr = luv_addr_of_eio host port in
      Luv.TCP.bind (Handle.get "bind" sock) addr |> or_raise;
      listening_ip_socket ~backlog sock
    | `Unix path         ->
      let sock = Luv.Pipe.init ~loop:(get_loop ()) () |> or_raise |> Handle.of_luv ~sw in
      luv_reuse_addr sock reuse_addr;
      if reuse_addr then (
        match Unix.lstat path with
        | Unix.{ st_kind = S_SOCK; _ } -> Unix.unlink path
        | _ -> ()
        | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
      );
      Luv.Pipe.bind (Handle.get "bind" sock) path |> or_raise;
      (* Remove the path when done (except for abstract sockets). *)
      if String.length path > 0 && path.[0] <> Char.chr 0 then
        Switch.on_release sw (fun () -> Unix.unlink path);
      listening_unix_socket ~backlog sock

  method connect ~sw = function
    | `Tcp (host, port) ->
      let sock = Stream.connect_tcp ~sw (luv_addr_of_eio host port) in
      (socket sock :> < Eio.Flow.two_way; Eio.Flow.close >)
    | `Unix path ->
      let sock = Stream.connect_pipe ~sw path in
      (socket sock :> < Eio.Flow.two_way; Eio.Flow.close >)

  method datagram_socket ~reuse_addr ~reuse_port ~sw saddr =
    let domain = socket_domain_of saddr in
    let sock = Luv.UDP.init ~domain ~loop:(get_loop ()) () |> or_raise in
    let dg_sock = Handle.of_luv ~sw sock in
    begin match saddr with
    | `Udp (host, port) ->
      let addr = luv_addr_of_eio host port in
      luv_reuse_addr sock reuse_addr;
      luv_reuse_port sock reuse_port;
      Luv.UDP.bind sock addr |> or_raise
    | `UdpV4 | `UdpV6 -> ()
    end;
    udp_socket dg_sock

  method getaddrinfo = Low_level.getaddrinfo

  method getnameinfo = Eio_unix.getnameinfo
end

let secure_random =
  object
    inherit Eio.Flow.source

    method read_into buf =
      List.fold_left
        (fun dim ba -> Random.fill ba; dim + Bigarray.Array1.dim ba)
        0 (cstructv_to_luv [ buf ])
  end

type stdenv = <
  stdin : source;
  stdout : sink;
  stderr : sink;
  net : Eio.Net.t;
  domain_mgr : Eio.Domain_manager.t;
  clock : Eio.Time.clock;
  mono_clock : Eio.Time.Mono.t;
  fs : Eio.Fs.dir Eio.Path.t;
  cwd : Eio.Fs.dir Eio.Path.t;
  secure_random : Eio.Flow.source;
  debug : Eio.Debug.t;
>

let domain_mgr ~run_event_loop = object (self)
  inherit Eio.Domain_manager.t

  method run_raw (type a) fn =
    let domain_k : (unit Domain.t * a Suspended.t) option ref = ref None in
    let result = ref None in
    let async = Luv.Async.init ~loop:(get_loop ()) (fun async ->
        (* This is called in the parent domain after returning to the mainloop,
           so [domain_k] must be set by then. *)
        let domain, k = Option.get !domain_k in
        Log.debug (fun f -> f "Spawned domain finished (joining)");
        Domain.join domain;
        Luv.Handle.close async @@ fun () ->
        Suspended.continue_result k (Option.get !result)
      ) |> or_raise
    in
    enter @@ fun _st k ->
    let d = Domain.spawn (fun () ->
        result := Some (match fn () with
            | v -> Ok v
            | exception ex -> Error ex
          );
        Log.debug (fun f -> f "Sending finished notification");
        Luv.Async.send async |> or_raise
      ) in
    domain_k := Some (d, k)

  method run fn =
    self#run_raw (fun () ->
        let result = ref None in
        run_event_loop (fun _ ->
            result := Some (fn ())
          );
        Option.get !result
      )
end

let clock = object
  inherit Eio.Time.clock

  method now = Unix.gettimeofday ()
  method sleep_until = sleep_until
end

let mono_clock = object
  inherit Eio.Time.Mono.t

  method now = Mtime_clock.now ()

  method sleep_until time =
    let now = Mtime.to_uint64_ns (Mtime_clock.now ()) in
    let time = Mtime.to_uint64_ns time in
    if Int64.unsigned_compare now time >= 0 then Fiber.yield ()
    else (
      let delay_ns = Int64.sub time now |> Int64.to_float in
      let delay_ms = delay_ns /. 1e6 |> ceil |> truncate |> max 0 in
      Low_level.sleep_ms delay_ms
    )
end

type _ Eio.Generic.ty += Dir_resolve_new : (string -> string) Eio.Generic.ty
let dir_resolve_new x = Eio.Generic.probe x Dir_resolve_new

(* Warning: libuv doesn't provide [openat], etc, and so there is probably no way to make this safe.
   We make a best-efforts attempt to enforce the sandboxing using realpath and [`NOFOLLOW].
   todo: this needs more testing *)
class dir ~label (dir_path : string) = object (self)
  inherit Eio.Fs.dir

  val mutable closed = false

  method! probe : type a. a Eio.Generic.ty -> a option = function
    | Dir_resolve_new -> Some self#resolve_new
    | _ -> None

  (* Resolve a relative path to an absolute one, with no symlinks.
     @raise Eio.Fs.Permission_denied if it's outside of [dir_path]. *)
  method private resolve ?display_path path =
    if closed then Fmt.invalid_arg "Attempt to use closed directory %S" dir_path;
    let display_path = Option.value display_path ~default:path in
    if Filename.is_relative path then (
      let dir_path = File.realpath dir_path |> or_raise_path dir_path in
      let full = File.realpath (Filename.concat dir_path path) |> or_raise_path path in
      let prefix_len = String.length dir_path + 1 in
      if String.length full >= prefix_len && String.sub full 0 prefix_len = dir_path ^ Filename.dir_sep then
        full
      else if full = dir_path then
        full
      else
        raise (Eio.Fs.Permission_denied (display_path, Failure (Fmt.str "Path %S is outside of sandbox %S" full dir_path)))
    ) else (
      raise (Eio.Fs.Permission_denied (display_path, Failure (Fmt.str "Path %S is absolute" path)))
    )

  (* We want to create [path]. Check that the parent is in the sandbox. *)
  method private resolve_new path =
    let dir, leaf = Filename.dirname path, Filename.basename path in
    if leaf = ".." then Fmt.failwith "New path %S ends in '..'!" path
    else match self#resolve dir with
      | dir -> Filename.concat dir leaf
      | exception Eio.Fs.Not_found (dir, ex) ->
        raise (Eio.Fs.Not_found (Filename.concat dir leaf, ex))
      | exception Eio.Fs.Permission_denied (dir, ex) ->
        raise (Eio.Fs.Permission_denied (Filename.concat dir leaf, ex))

  method open_in ~sw path =
    let fd = File.open_ ~sw (self#resolve path) [`NOFOLLOW; `RDONLY] |> or_raise_path path in
    (flow fd :> <Eio.File.ro; Eio.Flow.close>)

  method open_out ~sw ~append ~create path =
    let mode, flags =
      match create with
      | `Never            -> 0,    []
      | `If_missing  perm -> perm, [`CREAT]
      | `Or_truncate perm -> perm, [`CREAT; `TRUNC]
      | `Exclusive   perm -> perm, [`CREAT; `EXCL]
    in
    let flags = if append then `APPEND :: flags else flags in
    let flags = `RDWR :: `NOFOLLOW :: flags in
    let real_path =
      if create = `Never then self#resolve path
      else self#resolve_new path
    in
    let fd = File.open_ ~sw real_path flags ~mode:[`NUMERIC mode] |> or_raise_path path in
    (flow fd :> <Eio.File.rw; Eio.Flow.close>)

  method open_dir ~sw path =
    Switch.check sw;
    let label = Filename.basename path in
    let d = new dir ~label (self#resolve path) in
    Switch.on_release sw (fun () -> d#close);
    d

  (* libuv doesn't seem to provide a race-free way to do this. *)
  method mkdir ~perm path =
    let real_path = self#resolve_new path in
    File.mkdir ~mode:[`NUMERIC perm] real_path |> or_raise_path path

  (* libuv doesn't seem to provide a race-free way to do this. *)
  method unlink path =
    let dir_path = Filename.dirname path in
    let leaf = Filename.basename path in
    let real_dir_path = self#resolve ~display_path:path dir_path in
    File.unlink (Filename.concat real_dir_path leaf) |> or_raise_path path

  (* libuv doesn't seem to provide a race-free way to do this. *)
  method rmdir path =
    let dir_path = Filename.dirname path in
    let leaf = Filename.basename path in
    let real_dir_path = self#resolve ~display_path:path dir_path in
    File.rmdir (Filename.concat real_dir_path leaf) |> or_raise_path path

  method read_dir path =
    let path = self#resolve path in
    File.readdir path |> or_raise_path path

  method rename old_path new_dir new_path =
    match dir_resolve_new new_dir with
    | None -> invalid_arg "Target is not a luv directory!"
    | Some new_resolve_new ->
      let old_path = self#resolve old_path in
      let new_path = new_resolve_new new_path in
      File.rename old_path new_path |> or_raise_path old_path

  method close = closed <- true

  method pp f = Fmt.string f (String.escaped label)
end

(* Full access to the filesystem. *)
let fs = object
  inherit dir ~label:"fs" "."

  (* No checks *)
  method! private resolve ?display_path:_ path = path
end

let cwd = object
  inherit dir  ~label:"cwd" "."
end

let stdenv ~run_event_loop =
  let stdin = lazy (source (File.of_luv_no_hook Luv.File.stdin ~close_unix:true)) in
  let stdout = lazy (sink (File.of_luv_no_hook Luv.File.stdout ~close_unix:true)) in
  let stderr = lazy (sink (File.of_luv_no_hook Luv.File.stderr ~close_unix:true)) in
  object (_ : stdenv)
    method stdin  = Lazy.force stdin
    method stdout = Lazy.force stdout
    method stderr = Lazy.force stderr
    method net = net
    method domain_mgr = domain_mgr ~run_event_loop
    method clock = clock
    method mono_clock = mono_clock
    method fs = (fs :> Eio.Fs.dir), "."
    method cwd = (cwd :> Eio.Fs.dir), "."
    method secure_random = secure_random
    method debug = Eio.Private.Debug.v
  end

let rec wakeup ~async ~io_queued run_q =
  match Lf_queue.pop run_q with
  | Some (Thread f) ->
    if not !io_queued then (
      Lf_queue.push run_q IO;
      io_queued := true;
    );
    f ();
    wakeup ~async ~io_queued run_q
  | Some IO ->
    (* If threads keep yielding they could prevent pending IO from being processed.
       Therefore, we keep an [IO] job on the queue to force us to check from time to time. *)
    io_queued := false;
    if not (Lf_queue.is_empty run_q) then
      Luv.Async.send async |> or_raise
  | None -> ()

let rec run : type a. (_ -> a) -> a = fun main ->
  Log.debug (fun l -> l "starting run");
  let loop = Luv.Loop.init () |> or_raise in
  let run_q = Lf_queue.create () in
  let io_queued = ref false in
  let async = Luv.Async.init ~loop (fun async ->
      try wakeup ~async ~io_queued run_q
      with ex ->
        let bt = Printexc.get_raw_backtrace () in
        Fmt.epr "Uncaught exception in run loop:@,%a@." Fmt.exn_backtrace (ex, bt);
        Luv.Loop.stop loop
    ) |> or_raise in
  let st = { loop; async; run_q; fd_map = Fd_map.empty } in
  let stdenv = stdenv ~run_event_loop:run in
  let rec fork ~new_fiber:fiber fn =
    Ctf.note_switch (Fiber_context.tid fiber);
    let open Effect.Deep in
    match_with fn ()
    { retc = (fun () -> Fiber_context.destroy fiber);
      exnc = (fun e -> Fiber_context.destroy fiber; raise e);
      effc = fun (type a) (e : a Effect.t) ->
        match e with
        | Await fn ->
          Some (fun k ->
            let k = { Suspended.k; fiber } in
            fn loop fiber (enqueue_thread st k))
        | Eio.Private.Effects.Fork (new_fiber, f) ->
          Some (fun k ->
              let k = { Suspended.k; fiber } in
              enqueue_at_head st k ();
              fork ~new_fiber f
            )
        | Eio.Private.Effects.Get_context -> Some (fun k -> continue k fiber)
        | Enter_unchecked fn -> Some (fun k ->
            fn st { Suspended.k; fiber }
          )
        | Enter fn -> Some (fun k ->
            match Fiber_context.get_error fiber with
            | Some e -> discontinue k e
            | None -> fn st { Suspended.k; fiber }
          )
        | Eio.Private.Effects.Suspend fn ->
          Some (fun k ->
              let k = { Suspended.k; fiber } in
              fn fiber (enqueue_result_thread st k)
            )
        | Eio_unix.Private.Await_readable fd -> Some (fun k ->
            match Fiber_context.get_error fiber with
            | Some e -> discontinue k e
            | None ->
              let k = { Suspended.k; fiber } in
              Poll.await_readable st k fd
          )
        | Eio_unix.Private.Await_writable fd -> Some (fun k ->
            match Fiber_context.get_error fiber with
            | Some e -> discontinue k e
            | None ->
              let k = { Suspended.k; fiber } in
              Poll.await_writable st k fd
          )
        | Eio_unix.Private.Get_monotonic_clock -> Some (fun k -> continue k mono_clock)
        | Eio_unix.Private.Socket_of_fd (sw, close_unix, fd) -> Some (fun k ->
            try
              let fd = Low_level.Stream.of_unix fd in
              let sock = Luv.TCP.init ~loop () |> or_raise in
              let handle = Handle.of_luv ~sw ~close_unix sock in
              Luv.TCP.open_ sock fd |> or_raise;
              continue k (socket handle :> Eio_unix.socket)
            with Luv_error _ as ex ->
              discontinue k ex
          )
        | Eio_unix.Private.Socketpair (sw, domain, ty, protocol) -> Some (fun k ->
            try
              if domain <> Unix.PF_UNIX then failwith "Only PF_UNIX sockets are supported by libuv";
              let ty =
                match ty with
                | Unix.SOCK_DGRAM -> `DGRAM
                | Unix.SOCK_STREAM -> `STREAM
                | Unix.SOCK_RAW -> `RAW
                | Unix.SOCK_SEQPACKET -> failwith "Type SEQPACKET not support by libuv"
              in
              let a, b = Luv.TCP.socketpair ty protocol |> or_raise in
              let wrap x =
                let sock = Luv.TCP.init ~loop () |> or_raise in
                Luv.TCP.open_ sock x |> or_raise;
                let h = Handle.of_luv ~sw ~close_unix:true sock in
                (socket h :> Eio_unix.socket)
              in
              continue k (wrap a, wrap b)
            with Luv_error _ as ex ->
              discontinue k ex
          )
          | Eio_unix.Private.Pipe sw -> Some (fun k ->
            let r, w = Luv.Pipe.pipe ~read_flags:[] ~write_flags:[] () |> or_raise in
            let r = (flow (File.of_luv ~close_unix:true ~sw r) :> <Eio.Flow.source; Eio.Flow.close; Eio_unix.unix_fd>) in
            let w = (flow (File.of_luv ~close_unix:true ~sw w) :> <Eio.Flow.sink; Eio.Flow.close; Eio_unix.unix_fd>) in
            continue k (r, w)
          )
        | _ -> None
    }
  in
  let main_status = ref `Running in
  let new_fiber = Fiber_context.make_root () in
  fork ~new_fiber (fun () ->
      begin match main stdenv with
        | v -> main_status := `Done v
        | exception ex -> main_status := `Ex (ex, Printexc.get_raw_backtrace ())
      end;
      Luv.Loop.stop loop
    );
  ignore (Luv.Loop.run ~loop () : bool);
  Lf_queue.close st.run_q;
  Luv.Handle.close async (fun () -> Luv.Loop.close loop |> or_raise);
  match !main_status with
  | `Done v -> v
  | `Ex (ex, bt) -> Printexc.raise_with_backtrace ex bt
  | `Running -> failwith "Deadlock detected: no events scheduled but main function hasn't returned"
