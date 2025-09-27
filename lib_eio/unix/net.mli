open Eio.Std

(** {2 Types}

    These extend the types in {!Eio.Net} with support for file descriptors. *)

type stream_socket_ty   = [`Generic | `Unix] Eio.Net.stream_socket_ty
type datagram_socket_ty = [`Generic | `Unix] Eio.Net.datagram_socket_ty
type listening_socket_ty = [`Generic | `Unix] Eio.Net.listening_socket_ty
type 'a stream_socket = ([> stream_socket_ty] as 'a) r
type 'a datagram_socket = ([> datagram_socket_ty] as 'a) r
type 'a listening_socket = ([> listening_socket_ty] as 'a) r

type t = [`Generic | `Unix] Eio.Net.ty r

(** {2 Passing file descriptors} *)

val send_msg :
  [> `Platform of [>`Unix] | `Socket | `Stream] r ->
  ?fds:Fd.t list ->
  Cstruct.t list -> unit
(** Like {!Eio.Flow.write}, but allows passing file descriptors (for Unix-domain sockets). *)

val recv_msg_with_fds :
  [> `Platform of [>`Unix] | `Socket | `Stream] r ->
  sw:Switch.t ->
  max_fds:int ->
  Cstruct.t list ->
  int * Fd.t list
(** Like {!Eio.Flow.single_read}, but also allows receiving file descriptors (for Unix-domain sockets).

    @param max_fds The maximum number of file descriptors to accept (additional ones will be closed). *)

val fd : [> `Platform of [> `Unix] | `Socket] r -> Fd.t
(** [fd socket] is the underlying FD of [socket]. *)

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

val import_socket_stream : sw:Switch.t -> close_unix:bool -> Unix.file_descr -> [< `Unix_fd | stream_socket_ty] r
(** [import_socket_stream ~sw ~close_unix fd] is an Eio flow that uses [fd].

    It can be cast to e.g. {!source} for a one-way flow.
    The socket object will be closed when [sw] finishes.

    The [close_unix] and [sw] arguments are passed to {!Fd.of_unix}. *)

val import_socket_listening : sw:Switch.t -> close_unix:bool -> Unix.file_descr -> [< `Unix_fd | listening_socket_ty] r
(** [import_socket_listening ~sw ~close_unix fd] is an Eio listening socket that uses [fd].

    The socket object will be closed when [sw] finishes.

    The [close_unix] and [sw] arguments are passed to {!Fd.of_unix}. *)

val import_socket_datagram : sw:Switch.t -> close_unix:bool -> Unix.file_descr -> [< `Unix_fd | datagram_socket_ty] r
(** [import_socket_datagram ~sw ~close_unix fd] is an Eio datagram socket that uses [fd].

    The socket object will be closed when [sw] finishes.

    The [close_unix] and [sw] arguments are passed to {!Fd.of_unix}. *)

val socketpair_stream :
  sw:Switch.t ->
  ?domain:Unix.socket_domain ->
  ?protocol:int ->
  unit ->
  [< `Unix_fd | stream_socket_ty] r * [< `Unix_fd | stream_socket_ty] r
(** [socketpair_stream ~sw ()] returns a connected pair of flows, such that writes to one can be read by the other.

    This creates OS-level resources using [socketpair(2)].
    Note that, like all FDs created by Eio, they are both marked as close-on-exec by default. *)

val socketpair_datagram :
  sw:Switch.t ->
  ?domain:Unix.socket_domain ->
  ?protocol:int ->
  unit ->
  [< `Unix_fd | datagram_socket_ty] r * [< `Unix_fd | datagram_socket_ty] r
(** [socketpair_datagram ~sw ()] returns a connected pair of flows, such that writes to one can be read by the other.

    This creates OS-level resources using [socketpair(2)].
    Note that, like all FDs created by Eio, they are both marked as close-on-exec by default. *)

(** Socket options *)
module Sockopt : sig
  (** Unix-specific socket options.

      This module extends {!Eio.Net.Sockopt} with support for any Unix socket option. *)

  (** Generic wrappers for Unix socket options *)
  type _ Eio.Net.Sockopt.t +=
    | Unix_bool : Unix.socket_bool_option -> bool Eio.Net.Sockopt.t
        (** Wrap any Unix boolean socket option *)
    | Unix_int : Unix.socket_int_option -> int Eio.Net.Sockopt.t
        (** Wrap any Unix integer socket option *)
    | Unix_optint : Unix.socket_optint_option -> int option Eio.Net.Sockopt.t
        (** Wrap any Unix optional integer socket option *)
    | Unix_float : Unix.socket_float_option -> float Eio.Net.Sockopt.t
        (** Wrap any Unix float socket option *)

  val set : Fd.t -> 'a Eio.Net.Sockopt.t -> 'a -> unit
  (** [set fd opt v] sets socket option [opt] to value [v] on file descriptor [fd].

      Supports both portable options from {!Eio.Net.Sockopt} and Unix-specific options
      wrapped with [Unix_bool], [Unix_int], [Unix_optint], or [Unix_float].

      Example:
      {[
        (* Using portable options *)
        Eio.Net.setsockopt sock Eio.Net.Sockopt.SO_KEEPALIVE true;

        (* Using Unix-specific options *)
        let fd = Eio_unix.Net.fd sock in
        Eio_unix.Net.Sockopt.set fd (Unix_int Unix.SO_SNDBUF) 65536;
        Eio_unix.Net.Sockopt.set fd (Unix_optint Unix.SO_LINGER) (Some 10);
      ]} *)

  val get : Fd.t -> 'a Eio.Net.Sockopt.t -> 'a
  (** [get fd opt] gets the value of socket option [opt] on file descriptor [fd].

      Supports both portable options from {!Eio.Net.Sockopt} and Unix-specific options
      wrapped with [Unix_bool], [Unix_int], [Unix_optint], or [Unix_float]. *)
end

(** {2 Private API for backends} *)

val getnameinfo : Eio.Net.Sockaddr.t -> (string * string)
(** [getnameinfo sockaddr] returns domain name and service for [sockaddr]. *)

type _ Effect.t +=
  | Import_socket_stream :
      Switch.t * bool * Unix.file_descr -> [`Unix_fd | stream_socket_ty] r Effect.t     (** See {!import_socket_stream} *)
  | Import_socket_listening :
      Switch.t * bool * Unix.file_descr -> [`Unix_fd | listening_socket_ty] r Effect.t  (** See {!import_socket_listening} *)
  | Import_socket_datagram :
      Switch.t * bool * Unix.file_descr -> [`Unix_fd | datagram_socket_ty] r Effect.t   (** See {!import_socket_datagram} *)
  | Socketpair_stream : Eio.Switch.t * Unix.socket_domain * int ->
      ([`Unix_fd | stream_socket_ty] r * [`Unix_fd | stream_socket_ty] r) Effect.t      (** See {!socketpair_stream} *)
  | Socketpair_datagram : Eio.Switch.t * Unix.socket_domain * int ->
      ([`Unix_fd | datagram_socket_ty] r * [`Unix_fd | datagram_socket_ty] r) Effect.t  (** See {!socketpair_datagram} *)
[@@alert "-unstable"]
