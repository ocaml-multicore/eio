open Eio.Std

(** {2 Types}

    These extend the types in {!Eio.Net} with support for file descriptors. *)

class virtual stream_socket : object (<Resource.t; Eio.Flow.close; ..>)
  inherit Eio.Net.stream_socket
end

class virtual datagram_socket : object (<Resource.t; Eio.Flow.close; ..>)
  inherit Eio.Net.datagram_socket
end

class virtual t : object
  inherit Eio.Net.t

  method getnameinfo : Eio.Net.Sockaddr.t -> (string * string)
end

(** {2 Unix address conversions}

    Note: OCaml's {!Unix.sockaddr} type considers e.g. TCP port 80 and UDP port
    80 to be the same thing, whereas Eio regards them as separate addresses
    that just happen to have the same representation (a host address and a port
    number), so we have separate "of_unix" functions for each. *)

val sockaddr_to_unix : [< Eio.Net.Sockaddr.stream | Eio.Net.Sockaddr.datagram] -> Unix.sockaddr
val sockaddr_of_unix_stream : Unix.sockaddr -> Eio.Net.Sockaddr.stream
val sockaddr_of_unix_datagram : Unix.sockaddr -> Eio.Net.Sockaddr.datagram

(** Convert between Eio.Net.Ipaddr and Unix.inet_addr. *)
module Ipaddr : sig
  (** Internally, these are actually the same type, so these are just casts. *)

  val to_unix : [< `V4 | `V6] Eio.Net.Ipaddr.t -> Unix.inet_addr
  val of_unix : Unix.inet_addr -> Eio.Net.Ipaddr.v4v6
end

(** {2 Creating or importing sockets} *)

val import_socket_stream : sw:Switch.t -> close_unix:bool -> Unix.file_descr -> stream_socket
(** [import_socket_stream ~sw ~close_unix:true fd] is an Eio flow that uses [fd].

    It can be cast to e.g. {!source} for a one-way flow.
    The socket object will be closed when [sw] finishes.

    The [close_unix] and [sw] arguments are passed to {!Fd.of_unix}. *)

val import_socket_datagram : sw:Switch.t -> close_unix:bool -> Unix.file_descr -> datagram_socket
(** [import_socket_datagram ~sw ~close_unix:true fd] is an Eio datagram socket that uses [fd].

    The socket object will be closed when [sw] finishes.

    The [close_unix] and [sw] arguments are passed to {!Fd.of_unix}. *)

val socketpair_stream :
  sw:Switch.t ->
  ?domain:Unix.socket_domain ->
  ?protocol:int ->
  unit ->
  stream_socket * stream_socket
(** [socketpair_stream ~sw ()] returns a connected pair of flows, such that writes to one can be read by the other.

    This creates OS-level resources using [socketpair(2)].
    Note that, like all FDs created by Eio, they are both marked as close-on-exec by default. *)

val socketpair_datagram :
  sw:Switch.t ->
  ?domain:Unix.socket_domain ->
  ?protocol:int ->
  unit ->
  datagram_socket * datagram_socket
(** [socketpair_datagram ~sw ()] returns a connected pair of flows, such that writes to one can be read by the other.

    This creates OS-level resources using [socketpair(2)].
    Note that, like all FDs created by Eio, they are both marked as close-on-exec by default. *)

(** {2 Private API for backends} *)

val getnameinfo : Eio.Net.Sockaddr.t -> (string * string)
(** [getnameinfo sockaddr] returns domain name and service for [sockaddr]. *)

type _ Effect.t +=
  | Import_socket_stream :
      Switch.t * bool * Unix.file_descr -> stream_socket Effect.t       (** See {!import_socket_stream} *)
  | Import_socket_datagram :
      Switch.t * bool * Unix.file_descr -> datagram_socket Effect.t     (** See {!import_socket_datagram} *)
  | Socketpair_stream : Eio.Switch.t * Unix.socket_domain * int ->
      (stream_socket * stream_socket) Effect.t                    (** See {!socketpair_stream} *)
  | Socketpair_datagram : Eio.Switch.t * Unix.socket_domain * int ->
      (datagram_socket * datagram_socket) Effect.t                (** See {!socketpair_datagram} *)
[@@alert "-unstable"]
