(** Effects based parallel IO for OCaml.

    Eio provides support for concurrency (juggling many tasks) and
    parallelism (using multiple CPU cores for performance).

    It provides facilities for creating and coordinating fibers (light-weight
    threads) and domains (for parallel processing), as well as interfaces for
    interacting with resources provided by the operating system.

    These features must be used within an {e event loop},
    provided by an Eio {e backend}.
    Applications can use {!Eio_main.run} to run a suitable loop.

    See {{:https://github.com/ocaml-multicore/eio}} for a tutorial. *)

(** Commonly used standard features. This module is intended to be [open]ed. *)
module Std = Std

(** {1 Fibers} *)

(** Grouping fibers and other resources so they can be turned off together. *)
module Switch = Eio__core.Switch

(** A fiber is a light-weight thread. *)
module Fiber = Eio__core.Fiber

(** Cancelling fibers. *)
module Cancel = Eio__core.Cancel

(** {1 Concurrency primitives} *)

(** A promise is a placeholder for result that will arrive in the future. *)
module Promise = Eio__core.Promise

(** A counting semaphore. *)
module Semaphore = Semaphore

(** Mutual exclusion. *)
module Mutex = Eio_mutex

(** Waiting for a condition to become true. *)
module Condition = Condition

(** Delayed evaluation. *)
module Lazy = Lazy

(** {1 Collections} *)

(** A stream/queue. *)
module Stream = Stream

(** A pool of resources. *)
module Pool = Pool

(** {1 Multiple domains} *)

(** Parallel computation across multiple CPU cores. *)
module Domain_manager = Domain_manager

(** A pool of domains for executing jobs. *)
module Executor_pool = Executor_pool

(** {1 Errors and debugging} *)

val traceln :
  ?__POS__:string * int * int * int ->
  ('a, Format.formatter, unit, unit) format4 -> 'a
(** [traceln fmt] outputs a debug message (typically to stderr).

    Trace messages are printed by default and do not require logging to be configured first.
    The message is printed with a newline, and is flushed automatically.
    [traceln] is intended for quick debugging rather than for production code.

    Unlike most Eio operations, [traceln] will never switch to another fiber;
    if the OS is not ready to accept the message then the whole domain waits.

    It is safe to call [traceln] from multiple domains at the same time.
    Each line will be written atomically.

    Examples:
    {[
      traceln "x = %d" x;
      traceln "x = %d" x ~__POS__;   (* With location information *)
    ]}
    @param __POS__ Display [__POS__] as the location of the [traceln] call. *)

(** Eio exceptions. *)
module Exn = Eio__core.Exn

exception Io of Exn.err * Exn.context

(** Control over debugging. *)
module Debug : sig
  (** Example:
      {[
        open Eio.Std

        let my_traceln = {
          Eio.Debug.traceln = fun ?__POS__:_ fmt -> Fmt.epr ("[custom-trace] " ^^ fmt ^^ "@.")
        }

        let () =
          Eio_main.run @@ fun env ->
          let debug = Eio.Stdenv.debug env in
          Fiber.with_binding debug#traceln my_traceln @@ fun () ->
          traceln "Traced with custom function"
      ]}

      This will output:

      {[ [custom-trace] Traced with custom function ]}
  *)

  type traceln = Eio__core.Private.Debug.traceln = {
    traceln : 'a. ?__POS__:string * int * int * int -> ('a, Format.formatter, unit, unit) format4 -> 'a;
  } [@@unboxed]
  (** A function that writes trace logging to some trace output.

      It must not switch fibers, as tracing must not affect scheduling.
      If the system is not ready to receive the trace output,
      the whole domain must block until it is. *)

  val with_trace_prefix : (Format.formatter -> unit) -> (unit -> 'a) -> 'a
  (** [with_trace_prefix fmt fn] runs [fn ()] with a traceln that outputs [fmt] before each message. *)

  type t = <
    traceln : traceln Fiber.key;
  >
  (** Fiber keys used to control debugging. Use {!Stdenv.debug} to get this. *)
end

(** {1 Cross-platform OS API}

    The general pattern here is that each type of resource has a set of functions for using it,
    plus a provider ([Pi]) module to allow defining your own implementations.

    The system resources are available from the environment argument provided by your event loop
    (e.g. {!Eio_main.run}). *)

(** Defines the base resource type. *)
module Resource = Resource

(** {2 Byte streams} *)

(** A flow can be used to read or write bytes. *)
module Flow : sig
  include module type of Flow (** @inline *)

  (** {2 Convenience wrappers} *)

  val read_all : _ source -> string
  (** [read_all src] is a convenience wrapper to read an entire flow.

      It is the same as [Buf_read.(parse_exn take_all) src ~max_size:max_int] *)
end

(** Buffered input and parsing. *)
module Buf_read = Buf_read

(** Buffered output and formatting. *)
module Buf_write = Buf_write

(** {2 Networking} *)

(** Network sockets and addresses. *)
module Net = Net

(** {2 File-systems} *)

(** Accessing paths on a file-system. *)
module Path = Path

(** Operations on open files. *)
module File = File

(** File-system types. *)
module Fs = Fs

(** {2 Processes} *)

(** Managing child processes. *)
module Process = Process

(** {2 Time} *)

(** Clocks, time, sleeping and timeouts. *)
module Time = Time

(** {2 Main env} *)

(** The standard environment of a process. *)
module Stdenv : sig
  (** All access to the outside world comes from running the event loop,
      which provides an environment (e.g. an {!Eio_unix.Stdenv.base}).

      Example:
      {[
        let () =
          Eio_main.run @@ fun env ->
          Eio.Path.with_open_dir env#fs "/srv/www" @@ fun www ->
          serve_files www
            ~net:env#net
      ]}
  *)

  (** {1 Standard streams}

      To use these, see {!Flow}. *)

  val stdin  : <stdin  : _ Flow.source as 'a; ..> -> 'a
  val stdout : <stdout : _ Flow.sink   as 'a; ..> -> 'a
  val stderr : <stderr : _ Flow.sink   as 'a; ..> -> 'a

  (** {1 File-system access}

      To use these, see {!Path}. *)

  val cwd : <cwd : _ Path.t as 'a; ..> -> 'a
  (** [cwd t] is the current working directory of the process (this may change
      over time if the process does a "chdir" operation, which is not recommended). *)

  val fs : <fs : _ Path.t as 'a; ..> -> 'a
  (** [fs t] is the process's full access to the filesystem.

      Paths can be absolute or relative (to the current working directory).
      Using relative paths with this is similar to using them with {!cwd},
      except that this will follow ".." and symlinks to other parts of the filesystem.

      [fs] is useful for handling paths passed in by the user. *)

  (** {1 Network}

      To use this, see {!Net}.
  *)

  val net : <net : _ Net.t as 'a; ..> -> 'a
  (** [net t] gives access to the process's network namespace. *)

  (** {1 Processes }

      To use this, see {!Process}.
  *)

  val process_mgr : <process_mgr : _ Process.mgr as 'a; ..> -> 'a
  (** [process_mgr t] allows you to manage child processes. *)

  (** {1 Domains (using multiple CPU cores)}

      To use this, see {!Domain_manager}.
  *)

  val domain_mgr : <domain_mgr : _ Domain_manager.t as 'a; ..> -> 'a
  (** [domain_mgr t] allows running code on other cores. *)

  (** {1 Time}

      To use this, see {!Time}.
  *)

  val clock : <clock : _ Time.clock as 'a; ..> -> 'a
  (** [clock t] is the system clock (used to get the current time and date). *)

  val mono_clock : <mono_clock : _ Time.Mono.t as 'a; ..> -> 'a
  (** [mono_clock t] is a monotonic clock (used for measuring intervals). *)

  (** {1 Randomness} *)

  val secure_random : <secure_random : _ Flow.source as 'a; ..> -> 'a
  (** [secure_random t] is an infinite source of random bytes suitable for cryptographic purposes. *)

  (** {1 Debugging} *)

  val debug : <debug : <Debug.t; ..> as 'a; ..> -> 'a
  (** [debug t] provides privileged controls for debugging. *)

  val backend_id : <backend_id:string; ..> -> string
  (** [backend_id t] provides the name of the backend being used.

      The possible values are the same as the possible values of the "EIO_BACKEND"
      environment variable used by {!Eio_main.run}. *)
end

(** {1 Provider API for OS schedulers} *)

(** API for use by the scheduler implementation. *)
module Private = Eio__core.Private
