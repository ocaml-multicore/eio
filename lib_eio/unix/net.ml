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

class virtual stream_socket = object (_ : <Resource.t; ..>)
  inherit Eio.Net.stream_socket
end

class virtual datagram_socket = object (_ : <Resource.t; ..>)
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

type socket_int_option =
    EIO_TCP_CORK
  | EIO_TCP_KEEPCNT
  | EIO_TCP_KEEPIDLE
  | EIO_TCP_KEEPINTVL
  | EIO_TCP_DEFER_ACCEPT
  | EIO_TCP_NODELAY

external setsockopt_int : Unix.file_descr -> socket_int_option -> int -> unit =
  "eio_unix_setsockopt_int"
external getsockopt_int : Unix.file_descr -> socket_int_option -> int =
  "eio_unix_getsockopt_int"

module Sockopt = struct
  type _ Eio.Net.Sockopt.t +=
  | SO_KEEPALIVE : bool Eio.Net.Sockopt.t
  | SO_REUSEADDR : bool Eio.Net.Sockopt.t
  | SO_REUSEPORT : bool Eio.Net.Sockopt.t
  | TCP_NODELAY : bool Eio.Net.Sockopt.t
  | TCP_CORK : int Eio.Net.Sockopt.t
  | TCP_KEEPCNT : int Eio.Net.Sockopt.t
  | TCP_KEEPIDLE : int Eio.Net.Sockopt.t
  | TCP_KEEPINTVL : int Eio.Net.Sockopt.t
  | TCP_DEFER_ACCEPT : int Eio.Net.Sockopt.t

  let set : type a . Fd.t -> a Eio.Net.Sockopt.t -> a -> unit = fun sock k v ->
    Fd.use_exn "Sockopt.set" sock @@ fun fd ->
    match k with
    | TCP_CORK -> setsockopt_int fd EIO_TCP_CORK v
    | TCP_KEEPCNT -> setsockopt_int fd EIO_TCP_KEEPCNT v
    | TCP_KEEPIDLE -> setsockopt_int fd EIO_TCP_KEEPIDLE v
    | TCP_KEEPINTVL -> setsockopt_int fd EIO_TCP_KEEPINTVL v
    | TCP_DEFER_ACCEPT -> setsockopt_int fd EIO_TCP_DEFER_ACCEPT v
    | TCP_NODELAY -> setsockopt_int fd EIO_TCP_DEFER_ACCEPT (if v then 1 else 0)
    | SO_KEEPALIVE -> Unix.(setsockopt fd SO_KEEPALIVE v)
    | SO_REUSEADDR -> Unix.(setsockopt fd SO_REUSEADDR v)
    | SO_REUSEPORT -> Unix.(setsockopt fd SO_REUSEPORT v)
    | _ -> raise (Invalid_argument "TODO pp value")

  let get_descr : type a . Unix.file_descr -> a Eio.Net.Sockopt.t -> a = fun fd k ->
    match k with
    | TCP_CORK -> getsockopt_int fd EIO_TCP_CORK
    | TCP_KEEPCNT -> getsockopt_int fd EIO_TCP_KEEPCNT
    | TCP_KEEPIDLE -> getsockopt_int fd EIO_TCP_KEEPIDLE
    | TCP_KEEPINTVL -> getsockopt_int fd EIO_TCP_KEEPINTVL
    | TCP_DEFER_ACCEPT -> getsockopt_int fd EIO_TCP_DEFER_ACCEPT
    | TCP_NODELAY -> getsockopt_int fd EIO_TCP_NODELAY = 1
    | SO_KEEPALIVE -> Unix.(getsockopt fd SO_KEEPALIVE)
    | SO_REUSEADDR -> Unix.(getsockopt fd SO_REUSEADDR)
    | SO_REUSEPORT -> Unix.(getsockopt fd SO_REUSEPORT)
    | _ -> raise (Invalid_argument "TODO pp value")

  let get : type a . Fd.t -> a Eio.Net.Sockopt.t -> a = fun sock k ->
    Fd.use_exn "Sockopt.get" sock (fun fd -> get_descr fd k)
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
