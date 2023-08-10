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

val flow_handler :
  (module FLOW with type t = 't and type tag = 'tag) ->
  ('t, [`Unix_fd | 'tag Eio.Net.stream_socket_ty | Eio.File.rw_ty]) Eio.Resource.handler

module type DATAGRAM_SOCKET = sig
  include Eio.Net.Pi.DATAGRAM_SOCKET

  val fd : t -> Fd.t
end

val datagram_handler :
  (module DATAGRAM_SOCKET with type t = 't and type tag = 'tag) ->
  ('t, [`Unix_fd | 'tag Eio.Net.datagram_socket_ty]) Eio.Resource.handler

module type LISTENING_SOCKET = sig
  include Eio.Net.Pi.LISTENING_SOCKET

  val fd : t -> Fd.t
end

val listening_socket_handler :
  (module LISTENING_SOCKET with type t = 't and type tag = 'tag) ->
  ('t, [`Unix_fd | 'tag Eio.Net.listening_socket_ty]) Eio.Resource.handler
