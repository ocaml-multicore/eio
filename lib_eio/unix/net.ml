open Eio.Std

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

class virtual stream_socket = object (_ : <Resource.t; Eio.Flow.close; ..>)
  inherit Eio.Net.stream_socket
end

class virtual datagram_socket = object (_ : <Resource.t; Eio.Flow.close; ..>)
  inherit Eio.Net.datagram_socket
end

let getnameinfo (sockaddr : Eio.Net.Sockaddr.t) =
  let options =
    match sockaddr with
    | `Unix _ | `Tcp _ -> []
    | `Udp _ -> [Unix.NI_DGRAM]
  in
  let sockaddr = sockaddr_to_unix sockaddr in
  Private.run_in_systhread (fun () ->
    let Unix.{ni_hostname; ni_service} = Unix.getnameinfo sockaddr options in
    (ni_hostname, ni_service))

class virtual t = object
  inherit Eio.Net.t

  method getnameinfo = getnameinfo
end

[@@@alert "-unstable"]

type _ Effect.t +=
  | Import_socket_stream : Switch.t * bool * Unix.file_descr -> stream_socket Effect.t
  | Import_socket_datagram : Switch.t * bool * Unix.file_descr -> datagram_socket Effect.t
  | Socketpair_stream : Switch.t * Unix.socket_domain * int ->
      (stream_socket * stream_socket) Effect.t
  | Socketpair_datagram : Switch.t * Unix.socket_domain * int ->
      (datagram_socket * datagram_socket) Effect.t

let import_socket_stream ~sw ~close_unix fd = Effect.perform (Import_socket_stream (sw, close_unix, fd))

let import_socket_datagram ~sw ~close_unix fd = Effect.perform (Import_socket_datagram (sw, close_unix, fd))

let socketpair_stream ~sw ?(domain=Unix.PF_UNIX) ?(protocol=0) () =
  Effect.perform (Socketpair_stream (sw, domain, protocol))

let socketpair_datagram ~sw ?(domain=Unix.PF_UNIX) ?(protocol=0) () =
  Effect.perform (Socketpair_datagram (sw, domain, protocol))
