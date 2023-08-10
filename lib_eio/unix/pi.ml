open Eio.Std

module type STREAM_SOCKET = sig
  include Eio.Net.Pi.STREAM_SOCKET

  val send_msg : t -> fds:Fd.t list -> Cstruct.t list -> int
  val recv_msg_with_fds : t -> sw:Switch.t -> max_fds:int -> Cstruct.t list -> int * Fd.t list

  val fd : t -> Fd.t
end

type (_, _, _) Eio.Resource.pi +=
  | Stream_socket : ('t, (module STREAM_SOCKET with type t = 't), [> `Platform of [> `Unix] | `Socket | `Stream]) Eio.Resource.pi

module type FLOW = sig
  include Eio.File.Pi.WRITE
  include STREAM_SOCKET with type t := t
end

let flow_handler (type t tag) (module X : FLOW with type t = t and type tag = tag) : (t, _) Eio.Resource.handler =
  Eio.Resource.handler @@
  Eio.Resource.bindings (Eio.Net.Pi.stream_socket (module X)) @
  Eio.Resource.bindings (Eio.File.Pi.rw (module X)) @ [
    H (Resource.T, X.fd);
    H (Stream_socket, (module X));
  ]

module type DATAGRAM_SOCKET = sig
  include Eio.Net.Pi.DATAGRAM_SOCKET

  val fd : t -> Fd.t
end

let datagram_handler (type t tag) (module X : DATAGRAM_SOCKET with type t = t and type tag = tag) : (t, _) Eio.Resource.handler =
  Eio.Resource.handler @@
  Eio.Resource.bindings (Eio.Net.Pi.datagram_socket (module X)) @ [
    H (Resource.T, X.fd);
  ]

module type LISTENING_SOCKET = sig
  include Eio.Net.Pi.LISTENING_SOCKET

  val fd : t -> Fd.t
end

let listening_socket_handler (type t tag) (module X : LISTENING_SOCKET with type t = t and type tag = tag)
  : (t, _) Eio.Resource.handler =
  Eio.Resource.handler @@
  Eio.Resource.bindings (Eio.Net.Pi.listening_socket (module X)) @ [
    H (Resource.T, X.fd);
  ]
