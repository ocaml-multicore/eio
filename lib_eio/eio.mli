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

(** {1 Concurrency primitives} *)

(** Grouping fibers and other resources so they can be turned off together. *)
module Switch = Eio__core.Switch

(** A promise is a placeholder for result that will arrive in the future. *)
module Promise = Eio__core.Promise

(** A fiber is a light-weight thread. *)
module Fiber = Eio__core.Fiber

(**/**)
module Fibre = Fiber [@@deprecated "Now spelt Fiber"]
(**/**)

(** A counting semaphore. *)
module Semaphore = Semaphore

(** Mutual exclusion. *)
module Mutex = Eio_mutex

(** Waiting for a condition to become true. *)
module Condition = Condition

(** A stream/queue. *)
module Stream = Stream

(** Cancelling fibers. *)
module Cancel = Eio__core.Cancel

(** Commonly used standard features. This module is intended to be [open]ed. *)
module Std : sig
  module Promise = Promise
  module Fiber = Fiber
  (**/**)
  module Fibre = Fiber [@@deprecated "Now spelt Fiber"]
  (**/**)
  module Switch = Switch

  val traceln :
    ?__POS__:string * int * int * int ->
    ('a, Format.formatter, unit, unit) format4 -> 'a
    (** Same as {!Eio.traceln}. *)
end

(** {1 Cross-platform OS API}

    The general pattern here is that each type of resource has a set of functions for using it,
    plus an object type to allow defining your own implementations.
    To use the resources, it is recommended that you use the functions rather than calling
    methods directly. Using the functions results in better error messages from the compiler,
    and may provide extra features or sanity checks.

    The system resources are available from the {!Stdenv.t} provided by your event loop
    (e.g. {!Eio_main.run}). *)

(** A base class for objects that can be queried at runtime for extra features. *)
module Generic = Generic

(** Byte streams. *)
module Flow = Flow

(** Buffered input and parsing *)
module Buf_read = Buf_read

(** Buffered output *)
module Buf_write = Buf_write

(** Networking. *)
module Net = Net

(** Parallel computation across multiple CPU cores. *)
module Domain_manager : sig
  class virtual t : object
    method virtual run_raw : 'a. (unit -> 'a) -> 'a

    method virtual run : 'a. (unit -> 'a) -> 'a
    (** Note: cancellation is handled by the {!run} wrapper function, not the object. *)
  end

  val run : #t -> (unit -> 'a) -> 'a
  (** [run t f] runs [f ()] in a newly-created domain and returns the result.

      Other fibers in the calling domain can run in parallel with the new domain.

      Warning: [f] must only access thread-safe values from the calling domain,
      but this is not enforced by the type system.

      If the calling fiber is cancelled, this is propagated to the spawned domain. *)

  val run_raw : #t -> (unit -> 'a) -> 'a
  (** [run_raw t f] is like {!run}, but does not run an event loop in the new domain,
      and so cannot perform IO, fork fibers, etc. *)
end

(** Clocks, time, sleeping and timeouts. *)
module Time = Time

(** Operations on open files. *)
module File = File

(** File-system types. *)
module Fs = Fs

(** Accessing paths on a file-system. *)
module Path = Path

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

  type t = <
    traceln : traceln Fiber.key;
  >
  (** Fiber keys used to control debugging. Use {!Stdenv.debug} to get this. *)
end

(** The standard environment of a process. *)
module Stdenv : sig
  (** All access to the outside world comes from running the event loop,
      which provides a {!t}.

      Example:
      {[
        let () =
          Eio_main.run @@ fun env ->
          Eio.Path.with_open_dir env#fs "/srv/www" @@ fun www ->
          serve_files www
            ~net:env#net
      ]}
  *)

  type t = <
    stdin  : Flow.source;
    stdout : Flow.sink;
    stderr : Flow.sink;
    net : Net.t;
    domain_mgr : Domain_manager.t;
    clock : Time.clock;
    fs : Fs.dir Path.t;
    cwd : Fs.dir Path.t;
    secure_random : Flow.source;
    debug : Debug.t;
  >

  (** {1 Standard streams}

      To use these, see {!Flow}. *)

  val stdin  : <stdin  : #Flow.source as 'a; ..> -> 'a
  val stdout : <stdout : #Flow.sink   as 'a; ..> -> 'a
  val stderr : <stderr : #Flow.sink   as 'a; ..> -> 'a

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

  val net : <net : #Net.t as 'a; ..> -> 'a
  (** [net t] gives access to the process's network namespace. *)

  (** {1 Domains (using multiple CPU cores)}

      To use this, see {!Domain_manager}.
  *)

  val domain_mgr : <domain_mgr : #Domain_manager.t as 'a; ..> -> 'a
  (** [domain_mgr t] allows running code on other cores. *)

  (** {1 Time}

      To use this, see {!Time}.
  *)

  val clock : <clock : #Time.clock as 'a; ..> -> 'a
  (** [clock t] is the system clock. *)

  (** {1 Randomness} *)

  val secure_random : <secure_random : #Flow.source as 'a; ..> -> 'a
  (** [secure_random t] is a source of random bytes suitable for cryptographic purposes. *)

  (** {1 Debugging} *)

  val debug : <debug : <Debug.t; ..> as 'a; ..> -> 'a
  (** [debug t] provides privileged controls for debugging. *)
end

(** {1 Errors and Debugging} *)

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

(** Reporting multiple failures at once. *)
module Exn = Eio__core.Exn

(** {1 Provider API for OS schedulers} *)

(** API for use by the scheduler implementation. *)
module Private = Eio__core.Private
