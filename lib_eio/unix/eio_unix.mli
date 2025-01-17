(** Extension of {!Eio} for integration with OCaml's [Unix] module.

    Note that OCaml's [Unix] module is not safe, and therefore care must be taken when using these functions.
    For example, it is possible to leak file descriptors this way, or to use them after they've been closed,
    allowing one module to corrupt a file belonging to an unrelated module. *)

[@@@alert "-unstable"]

open Eio.Std

type Eio.Exn.Backend.t += Unix_error of Unix.error * string * string
(** Wrapper for embedding {!Unix.Unix_error} errors. *)

module Fd = Fd
(** A safe wrapper for {!Unix.file_descr}. *)

(** Eio resources backed by an OS file descriptor. *)
module Resource : sig
  type 'a t = ([> `Unix_fd] as 'a) Eio.Resource.t
  (** Resources that have FDs are tagged with [`Unix_fd]. *)

  type ('t, _, _) Eio.Resource.pi += T : ('t, 't -> Fd.t, [> `Unix_fd]) Eio.Resource.pi

  val fd : _ t -> Fd.t
  (** [fd t] returns the FD being wrapped by a resource. *)

  val fd_opt : _ Eio.Resource.t -> Fd.t option
  (** [fd_opt t] returns the FD being wrapped by a generic resource, if any.

      This just probes [t] using {!extension-FD}. *)
end

module Net = Net
(** Extended network API with support for file descriptors. *)

type source_ty = [`Unix_fd | Eio.Resource.close_ty | Eio.Flow.source_ty]
type sink_ty   = [`Unix_fd | Eio.Resource.close_ty | Eio.Flow.sink_ty]
type 'a source = ([> source_ty] as 'a) r
type 'a sink = ([> sink_ty] as 'a) r

val await_readable : Unix.file_descr -> unit
(** [await_readable fd] blocks until [fd] is readable (or has an error). *)

val await_writable : Unix.file_descr -> unit
(** [await_writable fd] blocks until [fd] is writable (or has an error). *)

val sleep : float -> unit
(** [sleep d] sleeps for [d] seconds, allowing other fibers to run.
    This is can be useful for debugging (e.g. to introduce delays to trigger a race condition)
    without having to plumb {!Eio.Stdenv.mono_clock} through your code.
    It can also be used in programs that don't care about tracking determinism. *)

val run_in_systhread : ?label:string -> (unit -> 'a) -> 'a
(** [run_in_systhread fn] runs the function [fn] using a pool of system threads ({! Thread.t}).

    This pool creates a new system thread if all threads are busy, it does not wait.
    [run_in_systhread] allows blocking calls to be made non-blocking.

    @param label The operation name to use in trace output. *)

val pipe : Switch.t -> source_ty r * sink_ty r
(** [pipe sw] returns a connected pair of flows [src] and [sink]. Data written to [sink]
    can be read from [src].
    Note that, like all FDs created by Eio, they are both marked as close-on-exec by default. *)

module Process = Process
(** Spawning child processes with extra control. *)

module Cap = Cap
(** Capsicum security. *)

(** The set of resources provided to a process on a Unix-compatible system. *)
module Stdenv : sig
  type base = <
    stdin  : source_ty r;
    stdout : sink_ty r;
    stderr : sink_ty r;
    net : [`Unix | `Generic] Eio.Net.ty r;
    domain_mgr : Eio.Domain_manager.ty r;
    process_mgr : Process.mgr_ty r;
    clock : float Eio.Time.clock_ty r;
    mono_clock : Eio.Time.Mono.ty r;
    fs : Eio.Fs.dir_ty Eio.Path.t;
    cwd : Eio.Fs.dir_ty Eio.Path.t;
    secure_random : Eio.Flow.source_ty r;
    debug : Eio.Debug.t;
    backend_id : string;
  >
  (** The common set of features provided by all traditional operating systems (BSDs, Linux, Mac, Windows).

      You can use the functions in {!Eio.Stdenv} to access these fields if you prefer. *)
end

(** API for Eio backends only. *)
module Private : sig
  type _ Effect.t +=
    | Await_readable : Unix.file_descr -> unit Effect.t      (** See {!await_readable} *)
    | Await_writable : Unix.file_descr -> unit Effect.t      (** See {!await_writable} *)
    | Get_monotonic_clock : Eio.Time.Mono.ty r Effect.t
    | Pipe : Eio.Switch.t -> (source_ty r * sink_ty r) Effect.t    (** See {!pipe} *)

  module Rcfd = Rcfd

  module Fork_action = Fork_action

  module Thread_pool = Thread_pool

  val read_link : Fd.t option -> string -> string
  val read_link_unix : Unix.file_descr option -> string -> string

  val chown : flags:int -> uid:int64 -> gid:int64 -> Fd.t -> string -> unit
  val chown_unix : flags:int -> uid:int64 -> gid:int64 -> Unix.file_descr -> string -> unit
end

module Pi = Pi
