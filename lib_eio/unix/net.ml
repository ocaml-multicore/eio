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

type _ Eio.Net.Sockopt.t +=
  | Sockopt_bool : Unix.socket_bool_option -> bool Eio.Net.Sockopt.t
  | Sockopt_int : Unix.socket_int_option -> int Eio.Net.Sockopt.t
  | Sockopt_optint : Unix.socket_optint_option -> int option Eio.Net.Sockopt.t
  | Sockopt_float : Unix.socket_float_option -> float Eio.Net.Sockopt.t

let () =
  let get : type a. a Eio.Net.Sockopt.t -> (string * a Fmt.t) option = function
    | Sockopt_bool SO_DEBUG -> Some ("Unix.SO_DEBUG", Fmt.bool)
    | Sockopt_bool SO_BROADCAST -> Some ("Unix.SO_BROADCAST", Fmt.bool)
    | Sockopt_bool SO_REUSEADDR -> Some ("Unix.SO_REUSEADDR", Fmt.bool)
    | Sockopt_bool SO_KEEPALIVE -> Some ("Unix.SO_KEEPALIVE", Fmt.bool)
    | Sockopt_bool SO_DONTROUTE -> Some ("Unix.SO_DONTROUTE", Fmt.bool)
    | Sockopt_bool SO_OOBINLINE -> Some ("Unix.SO_OOBINLINE", Fmt.bool)
    | Sockopt_bool SO_ACCEPTCONN -> Some ("Unix.SO_ACCEPTCONN", Fmt.bool)
    | Sockopt_bool TCP_NODELAY -> Some ("Unix.TCP_NODELAY", Fmt.bool)
    | Sockopt_bool IPV6_ONLY -> Some ("Unix.IPV6_ONLY", Fmt.bool)
    | Sockopt_bool SO_REUSEPORT -> Some ("Unix.SO_REUSEPORT", Fmt.bool)
    | Sockopt_int SO_SNDBUF -> Some ("Unix.SO_SNDBUF", Fmt.int)
    | Sockopt_int SO_RCVBUF -> Some ("Unix.SO_RCVBUF", Fmt.int)
    | Sockopt_int SO_ERROR -> Some ("Unix.SO_ERROR", Fmt.int)
    | Sockopt_int SO_TYPE -> Some ("Unix.SO_TYPE", Fmt.int)
    | Sockopt_int SO_RCVLOWAT -> Some ("Unix.SO_RCVLOWAT", Fmt.int)
    | Sockopt_int SO_SNDLOWAT -> Some ("Unix.SO_SNDLOWAT", Fmt.int)
    | Sockopt_optint SO_LINGER -> Some ("Unix.SO_LINGER", Fmt.(option ~none:(any "<none>") int))
    | Sockopt_float SO_RCVTIMEO -> Some ("Unix.SO_RCVTIMEO", Fmt.float)
    | Sockopt_float SO_SNDTIMEO -> Some ("Unix.SO_SNDTIMEO", Fmt.float)
    | _ -> None
  [@@alert "-deprecated"]
  in
  Eio.Net.Sockopt.register_printer { get }

let setsockopt : type a. Fd.t -> a Eio.Net.Sockopt.t -> a -> unit = fun fd opt v ->
  Fd.use_exn "setsockopt" fd @@ fun fd ->
  match opt with
  | Eio.Net.Sockopt.SO_DEBUG -> Unix.setsockopt fd Unix.SO_DEBUG v
  | Eio.Net.Sockopt.SO_BROADCAST -> Unix.setsockopt fd Unix.SO_BROADCAST v
  | Eio.Net.Sockopt.SO_REUSEADDR -> Unix.setsockopt fd Unix.SO_REUSEADDR v
  | Eio.Net.Sockopt.SO_KEEPALIVE -> Unix.setsockopt fd Unix.SO_KEEPALIVE v
  | Eio.Net.Sockopt.SO_DONTROUTE -> Unix.setsockopt fd Unix.SO_DONTROUTE v
  | Eio.Net.Sockopt.SO_OOBINLINE -> Unix.setsockopt fd Unix.SO_OOBINLINE v
  | Eio.Net.Sockopt.TCP_NODELAY -> Unix.setsockopt fd Unix.TCP_NODELAY v
  | Eio.Net.Sockopt.IPV6_ONLY -> Unix.setsockopt fd Unix.IPV6_ONLY v
  | Eio.Net.Sockopt.SO_REUSEPORT -> Unix.setsockopt fd Unix.SO_REUSEPORT v
  | Eio.Net.Sockopt.SO_SNDBUF -> Unix.setsockopt_int fd Unix.SO_SNDBUF v
  | Eio.Net.Sockopt.SO_RCVBUF -> Unix.setsockopt_int fd Unix.SO_RCVBUF v
  | Eio.Net.Sockopt.SO_RCVLOWAT -> Unix.setsockopt_int fd Unix.SO_RCVLOWAT v
  | Eio.Net.Sockopt.SO_SNDLOWAT -> Unix.setsockopt_int fd Unix.SO_SNDLOWAT v
  | Eio.Net.Sockopt.SO_LINGER -> Unix.setsockopt_optint fd Unix.SO_LINGER v
  | Eio.Net.Sockopt.SO_RCVTIMEO -> Unix.setsockopt_float fd Unix.SO_RCVTIMEO v
  | Eio.Net.Sockopt.SO_SNDTIMEO -> Unix.setsockopt_float fd Unix.SO_SNDTIMEO v
  | Sockopt_bool bo -> Unix.setsockopt fd bo v
  | Sockopt_int bo -> Unix.setsockopt_int fd bo v
  | Sockopt_optint bo -> Unix.setsockopt_optint fd bo v
  | Sockopt_float bo -> Unix.setsockopt_float fd bo v
  | _ -> raise (Eio.Net.err Invalid_option)

let getsockopt_descr : type a. Unix.file_descr -> a Eio.Net.Sockopt.t -> a = fun fd opt ->
  match opt with
  | Eio.Net.Sockopt.SO_DEBUG -> Unix.getsockopt fd Unix.SO_DEBUG
  | Eio.Net.Sockopt.SO_BROADCAST -> Unix.getsockopt fd Unix.SO_BROADCAST
  | Eio.Net.Sockopt.SO_REUSEADDR -> Unix.getsockopt fd Unix.SO_REUSEADDR
  | Eio.Net.Sockopt.SO_KEEPALIVE -> Unix.getsockopt fd Unix.SO_KEEPALIVE
  | Eio.Net.Sockopt.SO_DONTROUTE -> Unix.getsockopt fd Unix.SO_DONTROUTE
  | Eio.Net.Sockopt.SO_OOBINLINE -> Unix.getsockopt fd Unix.SO_OOBINLINE
  | Eio.Net.Sockopt.TCP_NODELAY -> Unix.getsockopt fd Unix.TCP_NODELAY
  | Eio.Net.Sockopt.IPV6_ONLY -> Unix.getsockopt fd Unix.IPV6_ONLY
  | Eio.Net.Sockopt.SO_REUSEPORT -> Unix.getsockopt fd Unix.SO_REUSEPORT
  | Eio.Net.Sockopt.SO_SNDBUF -> Unix.getsockopt_int fd Unix.SO_SNDBUF
  | Eio.Net.Sockopt.SO_RCVBUF -> Unix.getsockopt_int fd Unix.SO_RCVBUF
  | Eio.Net.Sockopt.SO_RCVLOWAT -> Unix.getsockopt_int fd Unix.SO_RCVLOWAT
  | Eio.Net.Sockopt.SO_SNDLOWAT -> Unix.getsockopt_int fd Unix.SO_SNDLOWAT
  | Eio.Net.Sockopt.SO_LINGER -> Unix.getsockopt_optint fd Unix.SO_LINGER
  | Eio.Net.Sockopt.SO_RCVTIMEO -> Unix.getsockopt_float fd Unix.SO_RCVTIMEO
  | Eio.Net.Sockopt.SO_SNDTIMEO -> Unix.getsockopt_float fd Unix.SO_SNDTIMEO
  | Sockopt_bool bo -> Unix.getsockopt fd bo
  | Sockopt_int bo -> Unix.getsockopt_int fd bo
  | Sockopt_optint bo -> Unix.getsockopt_optint fd bo
  | Sockopt_float bo -> Unix.getsockopt_float fd bo
  | _ -> raise (Eio.Net.err Invalid_option)

let getsockopt : type a. Fd.t -> a Eio.Net.Sockopt.t -> a = fun fd opt ->
  Fd.use_exn "getsockopt" fd @@ fun fd -> getsockopt_descr fd opt
