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

open Eio.Std
module Fd = Eio_unix.Fd

module Datagram_socket = struct
  type tag = [`Generic | `Unix]

  type t = {
    hook : Switch.hook;                 (* Deletes the path, for Unix-domain sockets *)
    fd : Fd.t;
  }

  let make ~hook fd = { hook; fd }

  let close t =
    Switch.remove_hook ~run:true t.hook;
    Fd.close t.fd

  let fd t = t.fd

  let send t ?dst buf =
    let dst = Option.map Eio_unix.Net.sockaddr_to_unix dst in
    let sent = Low_level.send_msg t.fd ?dst buf in
    assert (sent = Cstruct.lenv buf)

  let recv t buf =
    let addr, recv = Low_level.recv_msg t.fd [buf] in
    Eio_unix.Net.sockaddr_of_unix_datagram addr, recv

  let shutdown t cmd =
    Low_level.shutdown t.fd @@ match cmd with
    | `Receive -> Unix.SHUTDOWN_RECEIVE
    | `Send -> Unix.SHUTDOWN_SEND
    | `All -> Unix.SHUTDOWN_ALL

  let setsockopt t opt v = Low_level.setsockopt t.fd opt v
  let getsockopt t opt = Low_level.getsockopt t.fd opt
end

let datagram_handler = Eio_unix.Pi.datagram_handler (module Datagram_socket)

let datagram_socket ?(hook=Switch.null_hook) fd =
  Eio.Resource.T (Datagram_socket.make ~hook fd, datagram_handler)

module Listening_socket = struct
  type t = {
    hook : Switch.hook;                 (* Deletes the path, for Unix-domain sockets *)
    fd : Fd.t;
  }

  type tag = [`Generic | `Unix]

  let make ~hook fd = { hook; fd }

  let fd t = t.fd

  let close t =
    Switch.remove_hook ~run:true t.hook;
    Fd.close t.fd

  let accept t ~sw =
    Switch.check sw;
    let client, client_addr = Low_level.accept ~sw t.fd in
    let client_addr = match client_addr with
      | Unix.ADDR_UNIX path         -> `Unix path
      | Unix.ADDR_INET (host, port) -> `Tcp (Eio_unix.Net.Ipaddr.of_unix host, port)
    in
    let flow = (Flow.stream client :> _ Eio.Net.stream_socket) in
    flow, client_addr

  let listening_addr { fd; _ } =
    Eio_unix.Fd.use_exn "listening_addr" fd
      (fun fd -> Eio_unix.Net.sockaddr_of_unix_stream (Unix.getsockname fd))

  let setsockopt t opt v = Low_level.setsockopt t.fd opt v
  let getsockopt t opt = Low_level.getsockopt t.fd opt
end

let listening_handler = Eio_unix.Pi.listening_socket_handler (module Listening_socket)

let listening_socket ?(hook=Switch.null_hook) fd =
  Eio.Resource.T (Listening_socket.make ~hook fd, listening_handler)

let socket_domain_of = function
  | `Unix _ -> Unix.PF_UNIX
  | `UdpV4 -> Unix.PF_INET
  | `UdpV6 -> Unix.PF_INET6
  | `Udp (host, _)
  | `Tcp (host, _) ->
    Eio.Net.Ipaddr.fold host
      ~v4:(fun _ -> Unix.PF_INET)
      ~v6:(fun _ -> Unix.PF_INET6)

let connect ~bind_to ~options ~sw connect_addr =
  let addr = Eio_unix.Net.sockaddr_to_unix connect_addr in
  let sock = Low_level.socket ~sw (socket_domain_of connect_addr) Unix.SOCK_STREAM 0 in
  let rec set_options : Eio.Net.Sockopt.settings -> unit = function
    | [] -> ()
    | (o, v) :: tl -> Low_level.setsockopt sock o v; set_options tl
  in
  set_options options;
  Option.iter (fun a -> Low_level.bind sock (Eio_unix.Net.sockaddr_to_unix a)) bind_to;
  Low_level.connect sock addr;
  (Flow.stream sock :> _ Eio_unix.Net.stream_socket)

module Impl = struct
  type t = unit
  type tag = [`Unix | `Generic]

  let remove_on_release ~sw listen_addr =
    (* For Unix domain sockets, remove the path when done (except for abstract sockets). *)
    match listen_addr with
    | `Unix path ->
      if String.length path > 0 && path.[0] <> Char.chr 0 then
        Switch.on_release_cancellable sw (fun () -> Unix.unlink path)
      else Switch.null_hook
    | _ -> Switch.null_hook

  let listen () ~reuse_addr ~reuse_port ~backlog ~sw listen_addr =
    if reuse_addr then (
      match listen_addr with
      | `Tcp _ -> ()
      | `Unix path ->
        match Unix.lstat path with
        | Unix.{ st_kind = S_SOCK; _ } -> Unix.unlink path
        | _ -> ()
        | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
        | exception Unix.Unix_error (code, name, arg) -> raise @@ Err.v code name arg
    );
    let addr = Eio_unix.Net.sockaddr_to_unix listen_addr in
    let sock = Low_level.socket ~sw (socket_domain_of listen_addr) Unix.SOCK_STREAM 0 in
    if reuse_addr || reuse_port then (
      Fd.use_exn "setsockopt" sock (fun sock_unix ->
          if reuse_addr then Unix.setsockopt sock_unix Unix.SO_REUSEADDR true;
          if reuse_port then Unix.setsockopt sock_unix Unix.SO_REUSEPORT true);
    );
    Low_level.bind sock addr;
    let hook = remove_on_release ~sw listen_addr in
    Low_level.listen sock backlog;
    (listening_socket ~hook sock :> _ Eio.Net.listening_socket_ty r)

  let connect () ~bind_to ~options ~sw addr =
    (connect ~bind_to ~options ~sw addr :> [`Generic | `Unix] Eio.Net.stream_socket_ty r)

  let datagram_socket () ~reuse_addr ~reuse_port ~sw saddr =
    if reuse_addr then (
      match saddr with
      | `Udp _ | `UdpV4 | `UdpV6 -> ()
      | `Unix path ->
        match Unix.lstat path with
        | Unix.{ st_kind = S_SOCK; _ } -> Unix.unlink path
        | _ -> ()
        | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
        | exception Unix.Unix_error (code, name, arg) -> raise @@ Err.v code name arg
    );
    let sock = Low_level.socket ~sw (socket_domain_of saddr) Unix.SOCK_DGRAM 0 in
    let hook =
      match saddr with
      | `Udp _ | `Unix _ as saddr ->
        let addr = Eio_unix.Net.sockaddr_to_unix saddr in
        if reuse_addr || reuse_port then (
          Fd.use_exn "setsockopt" sock (fun sock_unix ->
              if reuse_addr then Unix.setsockopt sock_unix Unix.SO_REUSEADDR true;
              if reuse_port then Unix.setsockopt sock_unix Unix.SO_REUSEPORT true);
        );
        Low_level.bind sock addr;
        remove_on_release ~sw saddr
      | `UdpV4 | `UdpV6 -> Switch.null_hook
    in
    (datagram_socket ~hook sock :> [`Generic | `Unix] Eio.Net.datagram_socket_ty r)

  let getaddrinfo () = Low_level.getaddrinfo
  let getnameinfo () = Eio_unix.Net.getnameinfo
end

let v =
  let handler = Eio.Net.Pi.network (module Impl) in
  Eio.Resource.T ((), handler)
