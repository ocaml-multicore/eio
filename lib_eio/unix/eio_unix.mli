(** Extension of {!Eio} for integration with OCaml's [Unix] module.

    Note that OCaml's [Unix] module is not safe, and therefore care must be taken when using these functions.
    For example, it is possible to leak file descriptors this way, or to use them after they've been closed,
    allowing one module to corrupt a file belonging to an unrelated module. *)

val await_readable : Unix.file_descr -> unit
(** [await_readable fd] blocks until [fd] is readable (or has an error). *)

val await_writable : Unix.file_descr -> unit
(** [await_writable fd] blocks until [fd] is writable (or has an error). *)

(** Convert between [Unix.file_descr] and Eio objects. *)
module FD : sig
  val peek : #Eio.Generic.t -> Unix.file_descr option
  (** [peek x] is the Unix file descriptor underlying [x], if any.
      The caller must ensure that they do not continue to use the result after [x] is closed. *)

  val take : #Eio.Generic.t -> Unix.file_descr option
  (** [take x] is like [peek], but also marks [x] as closed on success (without actually closing the FD).
      [x] can no longer be used after this, and the caller is responsible for closing the FD. *)

  val as_socket : sw:Eio.Switch.t -> close_unix:bool -> Unix.file_descr -> < Eio.Flow.two_way; Eio.Flow.close >
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

(** API for Eio backends only. *)
module Private : sig
  open Eio.Private

  type _ Eio.Generic.ty += Unix_file_descr : [`Peek | `Take] -> Unix.file_descr Eio.Generic.ty
  (** See {!FD}. *)

  (** For Async_eio integration, we need to give Eio access to Async's lock. *)
  module Async : sig
    type t = {
      lock : unit -> unit;
      unlock : unit -> unit;
    }
  end

  type _ Effect.t += 
    | Await_readable : Unix.file_descr -> unit Effect.t      (** See {!await_readable} *)
    | Await_writable : Unix.file_descr -> unit Effect.t      (** See {!await_writable} *)
    | Get_system_clock : Eio.Time.clock Effect.t             (** See {!sleep} *)
    | Socket_of_fd : Eio.Switch.t * bool * Unix.file_descr ->
        < Eio.Flow.two_way; Eio.Flow.close > Effect.t        (** See {!FD.as_socket} *)
    | Set_async_integration : Async.t option -> unit Effect.t
end

module Ctf = Ctf_unix
