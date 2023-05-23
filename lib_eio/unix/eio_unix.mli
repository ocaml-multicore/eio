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
  type t = < fd : Fd.t >
  (** Resources that have FDs are sub-types of [t]. *)

  val fd : <t;..> -> Fd.t
  (** [fd t] returns the FD being wrapped by a resource. *)

  type _ Eio.Generic.ty += FD : Fd.t Eio.Generic.ty
  (** Resources that wrap FDs can handle this in their [probe] method to expose the FD. *)

  val fd_opt : #Eio.Generic.t -> Fd.t option
  (** [fd_opt t] returns the FD being wrapped by a generic resource, if any.

      This just probes [t] using {!extension-FD}. *)
end

module Net = Net
(** Extended network API with support for file descriptors. *)

type source = < Eio.Flow.source;  Resource.t; Eio.Flow.close >
type sink   = < Eio.Flow.sink;    Resource.t; Eio.Flow.close >
type socket = Net.stream_socket

val await_readable : Unix.file_descr -> unit
(** [await_readable fd] blocks until [fd] is readable (or has an error). *)

val await_writable : Unix.file_descr -> unit
(** [await_writable fd] blocks until [fd] is writable (or has an error). *)

(**/**)
module FD : sig
  val peek : < Resource.t; .. > -> Unix.file_descr
  [@@deprecated "Use Eio_unix.Resource.fd instead"]

  val peek_opt : #Eio.Generic.t -> Unix.file_descr option
  [@@deprecated "Use Eio_unix.Resource.fd_opt instead"]

  val take : < Resource.t; .. > -> Unix.file_descr
  [@@deprecated "Use Eio_unix.Resource.fd and Fd.remove instead"]

  val take_opt : #Eio.Generic.t -> Unix.file_descr option
  [@@deprecated "Use Eio_unix.Resource.fd_opt and Fd.remove instead"]

  val as_socket : sw:Switch.t -> close_unix:bool -> Unix.file_descr -> Net.stream_socket
  [@@deprecated "Use Eio_unix.Net.import_socket_stream instead"]
end

module Ipaddr = Net.Ipaddr
[@@deprecated "Use Eio_unix.Net.Ipaddr instead"]

val getnameinfo : Eio.Net.Sockaddr.t -> (string * string)
[@@deprecated "Use stdenv"]

val socketpair :
  sw:Switch.t ->
  ?domain:Unix.socket_domain ->
  ?ty:Unix.socket_type ->
  ?protocol:int ->
  unit ->
  Net.stream_socket * Net.stream_socket
[@@@deprecated "Use Net.socketpair_stream"]
(**/**)

val sleep : float -> unit
(** [sleep d] sleeps for [d] seconds, allowing other fibers to run.
    This is can be useful for debugging (e.g. to introduce delays to trigger a race condition)
    without having to plumb {!Eio.Stdenv.mono_clock} through your code.
    It can also be used in programs that don't care about tracking determinism. *)

val run_in_systhread : (unit -> 'a) -> 'a
(** [run_in_systhread fn] runs the function [fn] in a newly created system thread (a {! Thread.t}).
    This allows blocking calls to be made non-blocking. *)

val pipe : Switch.t -> source * sink
(** [pipe sw] returns a connected pair of flows [src] and [sink]. Data written to [sink]
    can be read from [src].
    Note that, like all FDs created by Eio, they are both marked as close-on-exec by default. *)

module Process = Process
(** Spawning child processes with extra control. *)

(** The set of resources provided to a process on a Unix-compatible system. *)
module Stdenv : sig
  type base = <
    stdin  : source;
    stdout : sink;
    stderr : sink;
    net : Eio.Net.t;
    domain_mgr : Eio.Domain_manager.t;
    process_mgr : Process.mgr;
    clock : Eio.Time.clock;
    mono_clock : Eio.Time.Mono.t;
    fs : Eio.Fs.dir Eio.Path.t;
    cwd : Eio.Fs.dir Eio.Path.t;
    secure_random : Eio.Flow.source;
    debug : Eio.Debug.t;
  >
  (** The common set of features provided by all traditional operating systems (BSDs, Linux, Mac, Windows).

      You can use the functions in {!Eio.Stdenv} to access these fields if you prefer. *)
end

(** API for Eio backends only. *)
module Private : sig
  type _ Effect.t += 
    | Await_readable : Unix.file_descr -> unit Effect.t      (** See {!await_readable} *)
    | Await_writable : Unix.file_descr -> unit Effect.t      (** See {!await_writable} *)
    | Get_monotonic_clock : Eio.Time.Mono.t Effect.t
    | Pipe : Eio.Switch.t -> (source * sink) Effect.t (** See {!pipe} *)

  module Rcfd = Rcfd

  module Fork_action = Fork_action
end

module Ctf = Ctf_unix
