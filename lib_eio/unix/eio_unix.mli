(** Extension of {!Eio} for integration with OCaml's [Unix] module.

    Note that OCaml's [Unix] module is not safe, and therefore care must be taken when using these functions.
    For example, it is possible to leak file descriptors this way, or to use them after they've been closed,
    allowing one module to corrupt a file belonging to an unrelated module. *)

open Eio.Std

type unix_fd = <
  unix_fd : [`Peek | `Take] -> Unix.file_descr;
>

type socket = <
  Eio.Flow.two_way;
  Eio.Flow.close;
  unix_fd;
>

val await_readable : Unix.file_descr -> unit
(** [await_readable fd] blocks until [fd] is readable (or has an error). *)

val await_writable : Unix.file_descr -> unit
(** [await_writable fd] blocks until [fd] is writable (or has an error). *)

(** Convert between [Unix.file_descr] and Eio objects. *)
module FD : sig
  val peek : < unix_fd; .. > -> Unix.file_descr
  (** [peek x] is the Unix file descriptor underlying [x].
      The caller must ensure that they do not continue to use the result after [x] is closed. *)

  val peek_opt : #Eio.Generic.t -> Unix.file_descr option
  (** [peek_opt x] is the Unix file descriptor underlying [x], if any.
      The caller must ensure that they do not continue to use the result after [x] is closed. *)

  val take : < unix_fd; .. > -> Unix.file_descr
  (** [take x] is like [peek], but also marks [x] as closed on success (without actually closing the FD).
      [x] can no longer be used after this, and the caller is responsible for closing the FD. *)

  val take_opt : #Eio.Generic.t -> Unix.file_descr option
  (** [take_opt x] is like [peek_opt], but also marks [x] as closed on success (without actually closing the FD).
      [x] can no longer be used after this, and the caller is responsible for closing the FD. *)

  val as_socket : sw:Switch.t -> close_unix:bool -> Unix.file_descr -> socket
  (** [as_socket ~sw ~close_unix:true fd] is an Eio flow that uses [fd].
      It can be cast to e.g. {!Eio.source} for a one-way flow.
      The socket object will be closed when [sw] finishes.
      @param close_unix If [true], closing the object will also close the underlying FD.
                        If [false], the caller is responsible for keeping [FD] open until the object is closed. *)
end

(** Convert between Eio.Net.Ipaddr and Unix.inet_addr. *)
module Ipaddr : sig
  (** Internally, these are actually the same type, so these are just casts. *)

  val to_unix : [< `V4 | `V6] Eio.Net.Ipaddr.t -> Unix.inet_addr
  val of_unix : Unix.inet_addr -> Eio.Net.Ipaddr.v4v6
end

val sleep : float -> unit
(** [sleep d] sleeps for [d] seconds, allowing other fibers to run.
    This is can be useful for debugging (e.g. to introduce delays to trigger a race condition)
    without having to plumb {!Eio.Stdenv.clock} through your code.
    It can also be used in programs that don't care about tracking determinism. *)

val run_in_systhread : (unit -> 'a) -> 'a
(** [run_in_systhread fn] runs the function [fn] in a newly created system thread (a {! Thread.t}).
    This allows blocking calls to be made non-blocking. *)

val socketpair :
  sw:Switch.t ->
  ?domain:Unix.socket_domain ->
  ?ty:Unix.socket_type ->
  ?protocol:int ->
  unit ->
  socket * socket
(** [socketpair ~sw ()] returns a connected pair of flows, such that writes to one can be read by the other.
    This creates OS-level resources using [socketpair(2)].
    Note that, like all FDs created by Eio, they are both marked as close-on-exec by default. *)

val pipe : Switch.t -> <Eio.Flow.source; Eio.Flow.close; unix_fd> * <Eio.Flow.sink; Eio.Flow.close; unix_fd>
(** [pipe sw] returns a connected pair of flows [src] and [sink]. Data written to [sink]
    can be read from [src].
    Note that, like all FDs created by Eio, they are both marked as close-on-exec by default. *)

(** API for Eio backends only. *)
module Private : sig
  type _ Eio.Generic.ty += Unix_file_descr : [`Peek | `Take] -> Unix.file_descr Eio.Generic.ty
  (** See {!FD}. *)

  type _ Effect.t += 
    | Await_readable : Unix.file_descr -> unit Effect.t      (** See {!await_readable} *)
    | Await_writable : Unix.file_descr -> unit Effect.t      (** See {!await_writable} *)
    | Get_system_clock : Eio.Time.clock Effect.t             (** See {!sleep} *)
    | Socket_of_fd : Switch.t * bool * Unix.file_descr ->
        socket Effect.t                                      (** See {!FD.as_socket} *)
    | Socketpair : Eio.Switch.t * Unix.socket_domain * Unix.socket_type * int ->
        (socket * socket) Effect.t                           (** See {!socketpair} *)
    | Pipe : Eio.Switch.t -> 
        (<Eio.Flow.source; Eio.Flow.close; unix_fd> * <Eio.Flow.sink; Eio.Flow.close; unix_fd>) Effect.t (** See {!pipe} *)
end

module Ctf = Ctf_unix

val getnameinfo : Eio.Net.Sockaddr.t -> (string * string)
(** [getnameinfo sockaddr] returns domain name and service for [sockaddr]. *)
