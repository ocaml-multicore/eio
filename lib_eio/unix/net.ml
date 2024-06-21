open Eio.Std

type stream_socket_ty   = [`Generic | `Unix] Eio.Net.stream_socket_ty
type datagram_socket_ty = [`Generic | `Unix] Eio.Net.datagram_socket_ty
type listening_socket_ty = [`Generic | `Unix] Eio.Net.listening_socket_ty
type 'a stream_socket = ([> stream_socket_ty] as 'a) r
type 'a datagram_socket = ([> datagram_socket_ty] as 'a) r
type 'a listening_socket = ([> listening_socket_ty] as 'a) r

module Ipaddr = struct
  let to_unix : _ Eio.Net.Ipaddr.t -> Unix.inet_addr = Obj.magic
  let of_unix : Unix.inet_addr -> _ Eio.Net.Ipaddr.t = Obj.magic
end

let sockaddr_to_unix = function
  | `Unix path -> Unix.ADDR_UNIX path
  | `Tcp (host, port) | `Udp (host, port) ->
    let host = Ipaddr.to_unix host in
    Unix.ADDR_INET (host, port)

let sockaddr_of_unix_stream = function
  | Unix.ADDR_UNIX path -> `Unix path
  | Unix.ADDR_INET (host, port) ->
    let host = Ipaddr.of_unix host in
    `Tcp (host, port)

let sockaddr_of_unix_datagram = function
  | Unix.ADDR_UNIX path -> `Unix path
  | Unix.ADDR_INET (host, port) ->
    let host = Ipaddr.of_unix host in
    `Udp (host, port)

let send_msg (Eio.Resource.T (t, ops)) ?(fds=[]) bufs =
  let module X = (val (Eio.Resource.get ops Pi.Stream_socket)) in
  let rec aux ~fds bufs =
    let sent = X.send_msg t ~fds bufs in
    match Cstruct.shiftv bufs sent with
    | [] -> ()
    | bufs -> aux bufs ~fds:[]
  in
  aux ~fds bufs

let recv_msg_with_fds (Eio.Resource.T (t, ops)) ~sw ~max_fds bufs =
  let module X = (val (Eio.Resource.get ops Pi.Stream_socket)) in
  X.recv_msg_with_fds t ~sw ~max_fds bufs

let getnameinfo (sockaddr : Eio.Net.Sockaddr.t) =
  let options =
    match sockaddr with
    | `Unix _ | `Tcp _ -> []
    | `Udp _ -> [Unix.NI_DGRAM]
  in
  let sockaddr = sockaddr_to_unix sockaddr in
  Thread_pool.run_in_systhread ~label:"getnameinfo" (fun () ->
    let Unix.{ni_hostname; ni_service} = Unix.getnameinfo sockaddr options in
    (ni_hostname, ni_service))

type t = [`Generic | `Unix] Eio.Net.ty r

[@@@alert "-unstable"]

type _ Effect.t +=
  | Import_socket_stream : Switch.t * bool * Unix.file_descr -> [`Unix_fd | stream_socket_ty] r Effect.t
  | Import_socket_listening : Switch.t * bool * Unix.file_descr -> [`Unix_fd | listening_socket_ty] r Effect.t
  | Import_socket_datagram : Switch.t * bool * Unix.file_descr -> [`Unix_fd | datagram_socket_ty] r Effect.t
  | Socketpair_stream : Switch.t * Unix.socket_domain * int ->
      ([`Unix_fd | stream_socket_ty] r * [`Unix_fd | stream_socket_ty] r) Effect.t
  | Socketpair_datagram : Switch.t * Unix.socket_domain * int ->
      ([`Unix_fd | datagram_socket_ty] r * [`Unix_fd | datagram_socket_ty] r) Effect.t

let open_stream s = (s : [`Unix_fd | stream_socket_ty] r :> [< `Unix_fd | stream_socket_ty] r)
let open_listening s = (s : [`Unix_fd | listening_socket_ty] r :> [< `Unix_fd | listening_socket_ty] r)
let open_datagram s = (s : [`Unix_fd | datagram_socket_ty] r :> [< `Unix_fd | datagram_socket_ty] r)

let import_socket_stream ~sw ~close_unix fd =
  open_stream @@ Effect.perform (Import_socket_stream (sw, close_unix, fd))

let import_socket_listening ~sw ~close_unix fd =
  open_listening @@ Effect.perform (Import_socket_listening (sw, close_unix, fd))

let import_socket_datagram ~sw ~close_unix fd =
  open_datagram @@ Effect.perform (Import_socket_datagram (sw, close_unix, fd))

let socketpair_stream ~sw ?(domain=Unix.PF_UNIX) ?(protocol=0) () =
  let a, b = Effect.perform (Socketpair_stream (sw, domain, protocol)) in
  (open_stream a, open_stream b)

let socketpair_datagram ~sw ?(domain=Unix.PF_UNIX) ?(protocol=0) () =
  let a, b = Effect.perform (Socketpair_datagram (sw, domain, protocol)) in
  (open_datagram a, open_datagram b)

let fd socket =
  Option.get (Resource.fd_opt socket)
