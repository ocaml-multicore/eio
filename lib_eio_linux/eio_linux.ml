(*
 * Copyright (C) 2020-2021 Anil Madhavapeddy
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

[@@@alert "-unstable"]

open Eio.Std

module Fiber_context = Eio.Private.Fiber_context
module Ctf = Eio.Private.Ctf

module Suspended = Eio_utils.Suspended
module Zzz = Eio_utils.Zzz
module Lf_queue = Eio_utils.Lf_queue

module Low_level = Low_level

(* Deprecated FD code *)
module FD = Fd
let get_fd = Eio_unix.Resource.fd
let get_fd_opt = Eio_unix.Resource.fd_opt

type _ Eio.Generic.ty += Dir_fd : Low_level.dir_fd Eio.Generic.ty
let get_dir_fd_opt t = Eio.Generic.probe t Dir_fd

type source = Eio_unix.source
type sink   = Eio_unix.sink

(* When copying between a source with an FD and a sink with an FD, we can share the chunk
   and avoid copying. *)
let fast_copy src dst =
  let fallback () =
    (* No chunks available. Use regular memory instead. *)
    let buf = Cstruct.create 4096 in
    try
      while true do
        let got = Low_level.readv src [buf] in
        Low_level.writev dst [Cstruct.sub buf 0 got]
      done
    with End_of_file -> ()
  in
  Low_level.with_chunk ~fallback @@ fun chunk ->
  let chunk_size = Uring.Region.length chunk in
  try
    while true do
      let got = Low_level.read_upto src chunk chunk_size in
      Low_level.write dst chunk got
    done
  with End_of_file -> ()

(* Try a fast copy using splice. If the FDs don't support that, switch to copying. *)
let _fast_copy_try_splice src dst =
  try
    while true do
      let _ : int = Low_level.splice src ~dst ~len:max_int in
      ()
    done
  with
  | End_of_file -> ()
  | Eio.Exn.Io (Eio.Exn.X Eio_unix.Unix_error ((EAGAIN | EINVAL), "splice", _), _) -> fast_copy src dst

(* XXX workaround for issue #319, PR #327 *)
let fast_copy_try_splice src dst = fast_copy src dst

(* Copy using the [Read_source_buffer] optimisation.
   Avoids a copy if the source already has the data. *)
let copy_with_rsb rsb dst =
  try
    while true do
      rsb (Low_level.writev_single dst)
    done
  with End_of_file -> ()

(* Copy by allocating a chunk from the pre-shared buffer and asking
   the source to write into it. This used when the other methods
   aren't available. *)
let fallback_copy src dst =
  let fallback () =
    (* No chunks available. Use regular memory instead. *)
    let buf = Cstruct.create 4096 in
    try
      while true do
        let got = Eio.Flow.single_read src buf in
        Low_level.writev dst [Cstruct.sub buf 0 got]
      done
    with End_of_file -> ()
  in
  Low_level.with_chunk ~fallback @@ fun chunk ->
  let chunk_cs = Uring.Region.to_cstruct chunk in
  try
    while true do
      let got = Eio.Flow.single_read src chunk_cs in
      Low_level.write dst chunk got
    done
  with End_of_file -> ()

let udp_socket sock = object
  inherit Eio.Net.datagram_socket

  method close = FD.close sock

  method send sockaddr buf =
    let addr = match sockaddr with
      | `Udp (host, port) ->
        let host = Eio_unix.Ipaddr.to_unix host in
        Unix.ADDR_INET (host, port)
    in
    Low_level.send_msg sock ~dst:addr [buf]

  method recv buf =
    let addr, recv = Low_level.recv_msg sock [buf] in
    match Uring.Sockaddr.get addr with
      | Unix.ADDR_INET (inet, port) ->
        `Udp (Eio_unix.Ipaddr.of_unix inet, port), recv
      | Unix.ADDR_UNIX _ ->
        raise (Failure "Expected INET UDP socket address but got Unix domain socket address.")
end

let flow fd =
  let is_tty = FD.use_exn "isatty" fd Unix.isatty in
  object (_ : <source; sink; ..>)
    method fd = fd
    method close = FD.close fd

    method stat = Low_level.fstat fd

    method probe : type a. a Eio.Generic.ty -> a option = function
      | Eio_unix.Resource.FD -> Some fd
      | _ -> None

    method read_into buf =
      if is_tty then (
        (* Work-around for https://github.com/axboe/liburing/issues/354
           (should be fixed in Linux 5.14) *)
        Low_level.await_readable fd
      );
      Low_level.readv fd [buf]

    method pread ~file_offset bufs =
      Low_level.readv ~file_offset fd bufs

    method pwrite ~file_offset bufs =
      Low_level.writev_single ~file_offset fd bufs

    method read_methods = []

    method write bufs = Low_level.writev fd bufs

    method copy src =
      match Eio_unix.Resource.fd_opt src with
      | Some src -> fast_copy_try_splice src fd
      | None ->
        let rec aux = function
          | Eio.Flow.Read_source_buffer rsb :: _ -> copy_with_rsb rsb fd
          | _ :: xs -> aux xs
          | [] -> fallback_copy src fd
        in
        aux (Eio.Flow.read_methods src)

    method shutdown cmd =
      FD.use_exn "shutdown" fd @@ fun fd ->
      Unix.shutdown fd @@ match cmd with
      | `Receive -> Unix.SHUTDOWN_RECEIVE
      | `Send -> Unix.SHUTDOWN_SEND
      | `All -> Unix.SHUTDOWN_ALL
  end

let source fd = (flow fd :> source)
let sink   fd = (flow fd :> sink)

let listening_socket fd = object
  inherit Eio.Net.listening_socket

  method close = FD.close fd

  method accept ~sw =
    Switch.check sw;
    let client, client_addr = Low_level.accept ~sw fd in
    let client_addr = match client_addr with
      | Unix.ADDR_UNIX path         -> `Unix path
      | Unix.ADDR_INET (host, port) -> `Tcp (Eio_unix.Ipaddr.of_unix host, port)
    in
    let flow = (flow client :> <Eio.Flow.two_way; Eio.Flow.close>) in
    flow, client_addr

  method! probe : type a. a Eio.Generic.ty -> a option = function
    | Eio_unix.Resource.FD -> Some fd
    | _ -> None
end

let socket_domain_of = function
  | `Unix _ -> Unix.PF_UNIX
  | `UdpV4 -> Unix.PF_INET
  | `UdpV6 -> Unix.PF_INET6
  | `Udp (host, _)
  | `Tcp (host, _) ->
    Eio.Net.Ipaddr.fold host
      ~v4:(fun _ -> Unix.PF_INET)
      ~v6:(fun _ -> Unix.PF_INET6)

let net = object
  inherit Eio.Net.t

  method listen ~reuse_addr ~reuse_port  ~backlog ~sw listen_addr =
    let socket_type, addr =
      match listen_addr with
      | `Unix path         ->
        if reuse_addr then (
          match Unix.lstat path with
          | Unix.{ st_kind = S_SOCK; _ } -> Unix.unlink path
          | _ -> ()
          | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
          | exception Unix.Unix_error (code, name, arg) -> raise @@ Err.wrap code name arg
        );
        Unix.SOCK_STREAM, Unix.ADDR_UNIX path
      | `Tcp (host, port)  ->
        let host = Eio_unix.Ipaddr.to_unix host in
        Unix.SOCK_STREAM, Unix.ADDR_INET (host, port)
    in
    let sock_unix = Unix.socket ~cloexec:true (socket_domain_of listen_addr) socket_type 0 in
    (* For Unix domain sockets, remove the path when done (except for abstract sockets). *)
    begin match listen_addr with
      | `Unix path ->
        if String.length path > 0 && path.[0] <> Char.chr 0 then
          Switch.on_release sw (fun () -> Unix.unlink path)
      | `Tcp _ -> ()
    end;
    if reuse_addr then
      Unix.setsockopt sock_unix Unix.SO_REUSEADDR true;
    if reuse_port then
      Unix.setsockopt sock_unix Unix.SO_REUSEPORT true;
    let sock = FD.of_unix ~sw ~seekable:false ~close_unix:true sock_unix in
    Unix.bind sock_unix addr;
    Unix.listen sock_unix backlog;
    listening_socket sock

  method connect ~sw connect_addr =
    let addr =
      match connect_addr with
      | `Unix path         -> Unix.ADDR_UNIX path
      | `Tcp (host, port)  ->
        let host = Eio_unix.Ipaddr.to_unix host in
        Unix.ADDR_INET (host, port)
    in
    let sock_unix = Unix.socket ~cloexec:true (socket_domain_of connect_addr) Unix.SOCK_STREAM 0 in
    let sock = FD.of_unix ~sw ~seekable:false ~close_unix:true sock_unix in
    Low_level.connect sock addr;
    (flow sock :> <Eio.Flow.two_way; Eio.Flow.close>)

  method datagram_socket ~reuse_addr ~reuse_port ~sw saddr =
    let sock_unix = Unix.socket ~cloexec:true (socket_domain_of saddr) Unix.SOCK_DGRAM 0 in
    let sock = FD.of_unix ~sw ~seekable:false ~close_unix:true sock_unix in
    begin match saddr with
    | `Udp (host, port) ->
      let host = Eio_unix.Ipaddr.to_unix host in
      let addr = Unix.ADDR_INET (host, port) in
      if reuse_addr then
        Unix.setsockopt sock_unix Unix.SO_REUSEADDR true;
      if reuse_port then
        Unix.setsockopt sock_unix Unix.SO_REUSEPORT true;
      Unix.bind sock_unix addr
    | `UdpV4 | `UdpV6 -> ()
    end;
    udp_socket sock

  method getaddrinfo = Low_level.getaddrinfo

  method getnameinfo = Eio_unix.getnameinfo
end

type stdenv = <
  stdin  : source;
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

let domain_mgr ~run_event_loop = object
  inherit Eio.Domain_manager.t

  method run_raw fn =
    let domain = ref None in
    Sched.enter (fun t k ->
        domain := Some (Domain.spawn (fun () -> Fun.protect fn ~finally:(fun () -> Sched.enqueue_thread t k ())))
      );
    Domain.join (Option.get !domain)

  method run fn =
    let domain = ref None in
    Sched.enter (fun t k ->
        let cancelled, set_cancelled = Promise.create () in
        Fiber_context.set_cancel_fn k.fiber (Promise.resolve set_cancelled);
        domain := Some (Domain.spawn (fun () ->
            Fun.protect
              (fun () ->
                 let result = ref None in
                 run_event_loop (fun () -> result := Some (fn ~cancelled)) ();
                 Option.get !result
              )
              ~finally:(fun () -> Sched.enqueue_thread t k ())))
      );
    Domain.join (Option.get !domain)
end

let mono_clock = object
  inherit Eio.Time.Mono.t

  method now = Mtime_clock.now ()

  method sleep_until = Low_level.sleep_until
end

let clock = object
  inherit Eio.Time.clock

  method now = Unix.gettimeofday ()

  method sleep_until time =
    (* todo: use the realtime clock directly instead of converting to monotonic time.
       That is needed to handle adjustments to the system clock correctly. *)
    let d = time -. Unix.gettimeofday () in
    Eio.Time.Mono.sleep mono_clock d
end

class dir ~label (fd : Low_level.dir_fd) = object
  inherit Eio.Fs.dir

  method! probe : type a. a Eio.Generic.ty -> a option = function
    | Dir_fd -> Some fd
    | _ -> None

  method open_in ~sw path =
    let fd = Low_level.openat ~sw fd path
        ~access:`R
        ~flags:Uring.Open_flags.cloexec
        ~perm:0
    in
    (flow fd :> <Eio.File.ro; Eio.Flow.close>)

  method open_out ~sw ~append ~create path =
    let perm, flags =
      match create with
      | `Never            -> 0,    Uring.Open_flags.empty
      | `If_missing  perm -> perm, Uring.Open_flags.creat
      | `Or_truncate perm -> perm, Uring.Open_flags.(creat + trunc)
      | `Exclusive   perm -> perm, Uring.Open_flags.(creat + excl)
    in
    let flags = if append then Uring.Open_flags.(flags + append) else flags in
    let fd = Low_level.openat ~sw fd path
        ~access:`RW
        ~flags:Uring.Open_flags.(cloexec + flags)
        ~perm
    in
    (flow fd :> <Eio.File.rw; Eio.Flow.close>)

  method open_dir ~sw path =
    let fd = Low_level.openat ~sw ~seekable:false fd path
        ~access:`R
        ~flags:Uring.Open_flags.(cloexec + path + directory)
        ~perm:0
    in
    let label = Filename.basename path in
    (new dir ~label (Low_level.FD fd) :> <Eio.Fs.dir; Eio.Flow.close>)

  method mkdir ~perm path = Low_level.mkdir_beneath ~perm fd path

  method read_dir path =
    Switch.run @@ fun sw ->
    let fd = Low_level.open_dir ~sw fd path in
    Low_level.read_dir fd

  method close =
    match fd with
    | FD x -> FD.close x
    | Cwd | Fs -> failwith "Can't close non-FD directory!"

  method unlink path = Low_level.unlink ~rmdir:false fd path
  method rmdir path = Low_level.unlink ~rmdir:true fd path

  method rename old_path t2 new_path =
    match get_dir_fd_opt t2 with
    | Some fd2 -> Low_level.rename fd old_path fd2 new_path
    | None -> raise (Unix.Unix_error (Unix.EXDEV, "rename-dst", new_path))

  method pp f = Fmt.string f (String.escaped label)
end

let secure_random = object
  inherit Eio.Flow.source
  method read_into buf = Low_level.getrandom buf; Cstruct.length buf
end

let stdenv ~run_event_loop =
  let stdin = lazy (source (Eio_unix.Fd.stdin)) in
  let stdout = lazy (sink (Eio_unix.Fd.stdout)) in
  let stderr = lazy (sink (Eio_unix.Fd.stderr)) in
  let fs = (new dir ~label:"fs" Fs, ".") in
  let cwd = (new dir ~label:"cwd" Cwd, ".") in
  object (_ : stdenv)
    method stdin  = Lazy.force stdin
    method stdout = Lazy.force stdout
    method stderr = Lazy.force stderr
    method net = net
    method domain_mgr = domain_mgr ~run_event_loop
    method clock = clock
    method mono_clock = mono_clock
    method fs = (fs :> Eio.Fs.dir Eio.Path.t)
    method cwd = (cwd :> Eio.Fs.dir Eio.Path.t)
    method secure_random = secure_random
    method debug = Eio.Private.Debug.v
  end

let run_event_loop (type a) ?fallback config (main : _ -> a) arg : a =
  Sched.with_sched ?fallback config @@ fun st ->
  let open Effect.Deep in
  let extra_effects : _ effect_handler = {
    effc = fun (type a) (e : a Effect.t) : ((a, Sched.exit) continuation -> Sched.exit) option ->
      match e with
      | Eio_unix.Private.Get_monotonic_clock -> Some (fun k -> continue k mono_clock)
      | Eio_unix.Private.Socket_of_fd (sw, close_unix, fd) -> Some (fun k ->
          let fd = FD.of_unix ~sw ~seekable:false ~close_unix fd in
          continue k (flow fd :> Eio_unix.socket)
        )
      | Eio_unix.Private.Socketpair (sw, domain, ty, protocol) -> Some (fun k ->
          match
            let a, b = Unix.socketpair ~cloexec:true domain ty protocol in
            let a = FD.of_unix ~sw ~seekable:false ~close_unix:true a |> flow in
            let b = FD.of_unix ~sw ~seekable:false ~close_unix:true b |> flow in
            ((a :> Eio_unix.socket), (b :> Eio_unix.socket))
          with
          | r -> continue k r
          | exception Unix.Unix_error (code, name, arg) ->
              discontinue k (Err.wrap code name arg)
        )
      | Eio_unix.Private.Pipe sw -> Some (fun k ->
          match
            let r, w = Low_level.pipe ~sw in
            let r = (flow r :> Eio_unix.source) in
            let w = (flow w :> Eio_unix.sink) in
            (r, w)
          with
          | r -> continue k r
          | exception Unix.Unix_error (code, name, arg) ->
            discontinue k (Err.wrap code name arg)
        )
      | _ -> None
  } in
  Sched.run ~extra_effects st main arg

let run ?queue_depth ?n_blocks ?block_size ?polling_timeout ?fallback main =
  let config = Sched.config ?queue_depth ?n_blocks ?block_size ?polling_timeout () in
  let stdenv = stdenv ~run_event_loop:(run_event_loop ?fallback:None config) in
  (* SIGPIPE makes no sense in a modern application. *)
  Sys.(set_signal sigpipe Signal_ignore);
  run_event_loop ?fallback config main stdenv
