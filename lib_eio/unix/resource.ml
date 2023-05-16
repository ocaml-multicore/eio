type 'a t = ([> `Unix_fd] as 'a) Eio.Resource.t

type ('t, _, _) Eio.Resource.pi += T : ('t, 't -> Fd.t, [> `Unix_fd]) Eio.Resource.pi
let fd (Eio.Resource.T (t, ops)) = Eio.Resource.get ops T t

let fd_opt (Eio.Resource.T (t, ops)) =
  match Eio.Resource.get_opt ops T with
  | Some f -> Some (f t)
  | None -> None

module type FLOW = sig
  include Eio.Net.Pi.STREAM_SOCKET
  include Eio.File.Pi.WRITE with type t := t
  include Net.Pi.STREAM_SOCKET with type t := t

  val fd : t -> Fd.t
end

let flow_handler (type t tag) (module X : FLOW with type t = t and type tag = tag) : (t, _) Eio.Resource.handler =
  Eio.Resource.handler @@
  Eio.Resource.bindings (Eio.Net.Pi.stream_socket (module X)) @
  Eio.Resource.bindings (Eio.File.Pi.rw (module X)) @ [
    H (T, X.fd);
    H (Net.Pi.Stream_socket, (module X));
  ]

module type DATAGRAM_SOCKET = sig
  include Eio.Net.Pi.DATAGRAM_SOCKET

  val fd : t -> Fd.t
end

let datagram_handler (type t tag) (module X : DATAGRAM_SOCKET with type t = t and type tag = tag) : (t, _) Eio.Resource.handler =
  Eio.Resource.handler @@
  Eio.Resource.bindings (Eio.Net.Pi.datagram_socket (module X)) @ [
    H (T, X.fd);
  ]

module type LISTENING_SOCKET = sig
  include Eio.Net.Pi.LISTENING_SOCKET

  val fd : t -> Fd.t
end

let listening_socket_handler (type t tag) (module X : LISTENING_SOCKET with type t = t and type tag = tag)
  : (t, _) Eio.Resource.handler =
  Eio.Resource.handler @@
  Eio.Resource.bindings (Eio.Net.Pi.listening_socket (module X)) @ [
    H (T, X.fd);
  ]
