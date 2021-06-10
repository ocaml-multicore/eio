(** Effects based parallel IO for OCaml *)

(** {1 Concurrency primitives} *)

(** Commonly used standard features. This module is intended to be [open]ed. *)
module Std : sig
  (** Controlling the lifetime of fibres (groups, exceptions, cancellations, timeouts). *)
  module Switch : sig
    type t
    (** A switch controls a group of fibres.
        Once a switch is turned off, all activities in that context should cancel themselves. *)

    exception Multiple_exceptions of exn list

    exception Cancelled of exn
    (** [Cancelled ex] indicates that the switch was turned off with exception [ex].
        It is usually not necessary to report a [Cancelled] exception to the user,
        as the original problem will be handled elsewhere. *)

    val top : (t -> 'a) -> 'a
    (** [top fn] runs [fn] with a fresh top-level switch (initially on).
        When [fn] exits, [top] waits for all operations registered with the switch to finish
        (it does not turn the switch off itself).
        If the switch is turned off before it returns, [top] re-raises the switch's exception(s).
        @raise Multiple_exceptions If [turn_off] is called more than once. *)

    val sub : ?on_release:(unit -> unit) -> sw:t -> on_error:(exn -> 'a) -> (t -> 'a) -> 'a
    (** [sub ~sw ~on_error fn] is like [top fn], but the new switch is a child of [sw], so that
        cancelling [sw] also cancels the child (but not the other way around).
        If [fn] raises an exception then it is passed to [on_error].
        If you only want to use [sub] to wait for a group of threads to finish, but not to contain
        errors, you can use [~on_error:raise].
        @param on_release Register this function with [Switch.on_release sub] once the sub-switch is created.
                          If creating the sub-switch fails, run it immediately. *)

    val check : t -> unit
    (** [check t] checks that [t] is still on.
        @raise Cancelled If the switch is off. *)

    val get_error : t -> exn option
    (** [get_error t] is like [check t] except that it returns the exception instead of raising it.
        If [t] is finished, this returns (rather than raising) the [Invalid_argument] exception too. *)

    val turn_off : t -> exn -> unit
    (** [turn_off t ex] turns off [t], with reason [ex].
        It returns immediately, without waiting for the shutdown actions to complete.
        If [t] is already off then [ex] is added to the list of exceptions (unless
        [ex] is [Cancelled] or identical to the original exception, in which case
        it is ignored). *)

    val on_release : t -> (unit -> unit) -> unit
    (** [on_release t fn] registers [fn] to be called once [t]'s main function has returned
        and all fibres have finished.
        If [fn] raises an exception, it is passed to [turn_off].
        Release handlers are run in LIFO order, in series.
        If you want to allow other release handlers to run concurrently, you can start the release
        operation and then call [on_release] again from within [fn] to register a function to await the result.
        This will be added to a fresh batch of handlers, run after the original set have finished.
        Note that [fn] must work even if the switch has been turned off,
        so using [sub t] or similar within [fn] is usually a bad idea. *)

    val on_release_cancellable : t -> (unit -> unit) -> (unit -> unit)
    (** Like [on_release], but returns a function that can be called to remove the handler.
        Calling this more than once has no effect. *)
  end

  module Promise : sig
    type 'a t
    (** An ['a t] is a promise for a value of type ['a]. *)

    type 'a u
    (** An ['a u] is a resolver for a promise of type ['a]. *)

    val create : ?label:string -> unit -> 'a t * 'a u
    (** [create ()] is a fresh promise/resolver pair.
        The promise is initially unresolved. *)

    val await : ?sw:Switch.t -> 'a t -> 'a
    (** [await t] blocks until [t] is resolved.
        If [t] is already resolved then this returns immediately.
        If [t] is broken, it raises the exception.
        @param sw Cancel wait if [sw] is turned off. *)

    val await_result : ?sw:Switch.t -> 'a t -> ('a, exn) result
    (** [await_result t] is like [await t], but returns [Error ex] if [t] is broken
        instead of raising an exception.
        Note that turning off [sw] still raises an exception. *)

    val fulfill : 'a u -> 'a -> unit
    (** [fulfill u v] successfully resolves [u]'s promise with the value [v].
        Any threads waiting for the result will be added to the run queue. *)

    val break : 'a u -> exn -> unit
    (** [break u ex] resolves [u]'s promise with the exception [ex].
        Any threads waiting for the result will be added to the run queue. *)

    val resolve : 'a t -> ('a, exn) result -> unit
    (** [resolve t (Ok x)] is [fulfill t x] and
        [resolve t (Error ex)] is [break t ex]. *)

    val fulfilled : 'a -> 'a t
    (** [fulfilled x] is a promise that is already fulfilled with result [x]. *)

    val broken : exn -> 'a t
    (** [broken x] is a promise that is already broken with exception [ex]. *)

    type 'a waiters

    type 'a state =
      | Unresolved of 'a waiters
      | Fulfilled of 'a
      | Broken of exn

    val state : 'a t -> 'a state
    (** [state t] is the current state of [t]. *)

    val is_resolved : 'a t -> bool
    (** [is_resolved t] is [true] iff [state t] is [Fulfilled] or [Broken]. *)

    val create_with_id : Ctf.id -> 'a t * 'a u
    (** Like [create], but the caller creates the tracing ID.
        This can be useful when implementing other primitives that use promises internally,
        to give them a different type in the trace output. *)
  end

  module Fibre : sig
    val both : sw:Switch.t -> (unit -> unit) -> (unit -> unit) -> unit
    (** [both ~sw f g] runs [f ()] and [g ()] concurrently.
        If either raises an exception, [sw] is turned off.
        [both] waits for both functions to finish even if one raises. *)

    val fork_ignore : sw:Switch.t -> (unit -> unit) -> unit
    (** [fork_ignore ~sw fn] runs [fn ()] in a new fibre, but does not wait for it to complete.
        The new fibre is attached to [sw] (which can't finish until the fibre ends).
        If the fibre raises an exception, [sw] is turned off.
        If [sw] is already off then [fn] fails immediately, but the calling thread continues. *)

    val fork_sub_ignore : ?on_release:(unit -> unit) -> sw:Switch.t -> on_error:(exn -> unit) -> (Switch.t -> unit) -> unit
    (** [fork_sub_ignore ~sw ~on_error fn] is like [fork_ignore], but it creates a new sub-switch for the fibre.
        This means that you can cancel the child switch without cancelling the parent.
        This is a convenience function for running {!Switch.sub} inside a {!fork_ignore}. *)

    val fork : sw:Switch.t -> exn_turn_off:bool -> (unit -> 'a) -> 'a Promise.t
    (** [fork ~sw ~exn_turn_off fn] starts running [fn ()] in a new fibre and returns a promise for its result.
        The new fibre is attached to [sw] (which can't finish until the fibre ends).
        @param exn_turn_off If [true] and [fn] raises an exception, [sw] is turned off (in addition to breaking the promise). *)

    val yield : ?sw:Switch.t -> unit -> unit
    (** [yield ()] asks the scheduler to switch to the next runnable task.
        The current task remains runnable, but goes to the back of the queue.
        @param sw Ensure that the switch is still on before returning. *)
  end

  val traceln :
    ?__POS__:string * int * int * int ->
    ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [traceln fmt] outputs a debug message (typically to stderr).
      Trace messages are printed by default and do not require logging to be configured first.
      The message is printed with a newline, and is flushed automatically.
      [traceln] is intended for quick debugging rather than for production code.

      Unlike most Eio operations, [traceln] will never switch to another fibre;
      if the OS is not ready to accept the message then the whole domain waits.

      It is safe to call [traceln] from multiple domains at the same time.
      Each line will be written atomically.

      Examples:
      {[
        traceln "x = %d" x;
        traceln "x = %d" x ~__POSS__;   (* With location information *)
      |}
      @param __POS__ Display [__POS__] as the location of the [traceln] call. *)
end

open Std

(** A counting semaphore for use within a single domain.
    The API is based on OCaml's [Semaphore.Counting]. *)
module Semaphore : sig
  type t
  (** The type of counting semaphores. *)

  val make : int -> t
  (** [make n] returns a new counting semaphore, with initial value [n].
      The initial value [n] must be nonnegative.
      @raise Invalid_argument if [n < 0] *)

  val release : t -> unit
  (** [release t] increments the value of semaphore [t].
      If other fibres are waiting on [t], the one that has been waiting the longest is resumed.
      @raise Sys_error if the value of the semaphore would overflow [max_int] *)

  val acquire : t -> unit
  (** [acquire t] blocks the calling fibre until the value of semaphore [t]
      is not zero, then atomically decrements the value of [t] and returns. *)

  val get_value : t -> int
  (** [get_value t] returns the current value of semaphore [t]. *)
end

(** {1 Cross-platform OS API} *)

(** A base class for objects that can be queried at runtime for extra features. *)
module Generic : sig
  type 'a ty = ..
  (** An ['a ty] is a query for a feature of type ['a]. *)

  class type t = object
    method probe : 'a. 'a ty -> 'a option
  end

  val probe : #t -> 'a ty -> 'a option
end

(** Byte streams. *)
module Flow : sig
  type shutdown_command = [ `Receive | `Send | `All ]

  class type close = object
    method close : unit
  end

  val close : #close -> unit

  class virtual read : object
    method virtual read_into : ?sw:Switch.t -> Cstruct.t -> int
  end

  val read_into : ?sw:Switch.t -> #read -> Cstruct.t -> int
  (** [read_into buf] reads one or more bytes into [buf].
      It returns the number of bytes written (which may be less than the
      buffer size even if there is more data to be read).
      [buf] must not be zero-length.
      @param sw Abort the read if [sw] is turned off.
      @raise End_of_file if there is no more data to read *)

  (** Producer base class. *)
  class virtual source : object
    inherit Generic.t
    inherit read
  end

  val string_source : string -> source

  val cstruct_source : Cstruct.t list -> source

  class virtual write : object
    method virtual write : 'a. ?sw:Switch.t -> (#source as 'a) -> unit
  end

  val copy : ?sw:Switch.t -> #source -> #write -> unit
  (** [copy src dst] copies data from [src] to [dst] until end-of-file. *)

  val copy_string : ?sw:Switch.t -> string -> #write -> unit

  (** Consumer base class. *)
  class virtual sink : object
    inherit Generic.t
    inherit write
  end

  val buffer_sink : Buffer.t -> sink

  (** Bidirectional stream base class. *)
  class virtual two_way : object
    inherit Generic.t
    inherit read
    inherit write

    method virtual shutdown : shutdown_command -> unit
  end

  val shutdown : #two_way -> shutdown_command -> unit
end

module Network : sig
  module Sockaddr : sig
    type inet_addr = Unix.inet_addr

    type t = [
      | `Unix of string
      | `Tcp of inet_addr * int
    ]

    val pp : Format.formatter -> t -> unit
  end

  module Listening_socket : sig
    class virtual t : object
      method virtual close : unit
      method virtual accept_sub :
        sw:Switch.t ->
        on_error:(exn -> unit) ->
        (sw:Switch.t -> <Flow.two_way; Flow.close> -> Sockaddr.t -> unit) ->
        unit
    end

    val accept_sub :
      sw:Switch.t ->
      #t ->
      on_error:(exn -> unit) ->
      (sw:Switch.t -> <Flow.two_way; Flow.close> -> Sockaddr.t -> unit) ->
      unit
    (** [accept t fn] waits for a new connection to [t] and then runs [fn ~sw flow client_addr] in a new fibre,
        created with [Fibre.fork_sub_ignore].
        [flow] will be closed automatically when the sub-switch is finished, if not already closed by then. *)
  end

  class virtual t : object
    method virtual listen : reuse_addr:bool -> backlog:int -> sw:Switch.t -> Sockaddr.t -> Listening_socket.t
    method virtual connect : sw:Switch.t -> Sockaddr.t -> <Flow.two_way; Flow.close>
  end

  val listen : ?reuse_addr:bool -> backlog:int -> sw:Switch.t -> #t -> Sockaddr.t -> Listening_socket.t
  (** [listen ~sw ~backlog t addr] is a new listening socket bound to local address [addr].
      The new socket will be closed when [sw] finishes, unless closed manually first.
      For (non-abstract) Unix domain sockets, the path will be removed afterwards.
      @param backlog The number of pending connections that can be queued up (see listen(2)).
      @param reuse_addr Set the [Unix.SO_REUSEADDR] socket option.
                        For Unix paths, also remove any stale left-over socket. *)

  val connect : sw:Switch.t -> #t -> Sockaddr.t -> <Flow.two_way; Flow.close>
  (** [connect ~sw t addr] is a new socket connected to remote address [addr].
      The new socket will be closed when [sw] finishes, unless closed manually first. *)
end

module Domain_manager : sig
  class virtual t : object
    method virtual run_compute_unsafe : 'a. (unit -> 'a) -> 'a
  end

  val run_compute_unsafe : #t -> (unit -> 'a) -> 'a
  (** [run_compute_unsafe t f] runs [f ()] in a newly-created domain and returns the result.
      The new domain does not get an event loop, and so cannot perform IO, fork fibres, etc.
      Other fibres in the calling domain can run in parallel with the new domain.
      Unsafe because [f] must only be able to access thread-safe values from the
      calling domain, but this is not enforced by the type system. *)
end

module Time : sig
  class virtual clock : object
    method virtual sleep : ?sw:Switch.t -> float -> unit
  end

  val sleep : ?sw:Switch.t -> #clock -> float -> unit
  (** [sleep t d] waits for [d] seconds.
      @param sw The sleep is aborted if the switch is turned off. *)
end

(** The standard environment of a process. *)
module Stdenv : sig
  type t = <
    stdin  : Flow.source;
    stdout : Flow.sink;
    stderr : Flow.sink;
    network : Network.t;
    domain_mgr : Domain_manager.t;
    clock : Time.clock;
  >

  val stdin  : <stdin  : #Flow.source as 'a; ..> -> 'a
  val stdout : <stdout : #Flow.sink   as 'a; ..> -> 'a
  val stderr : <stderr : #Flow.sink   as 'a; ..> -> 'a

  val network : <network : #Network.t as 'a; ..> -> 'a
  val domain_mgr : <domain_mgr : #Domain_manager.t as 'a; ..> -> 'a
  val clock : <clock : #Time.clock as 'a; ..> -> 'a
end

(** {1 Provider API for OS schedulers} *)

(** API for use by the scheduler implementation. *)
module Private : sig
  module Waiters : sig
    type 'a t
    (** A queue of callbacks waiting for a value of type ['a]. *)

    type waiter

    val null : waiter
    (** A dummy waiter that does nothing when removed. *)

    val add_waiter : 'a t -> (('a, exn) result -> unit) -> waiter
    (** [add_waiter t fn] adds [fn] to the queue of callbacks to be invoked when the wait is over.
        [fn] will typically add some saved continuation to the runqueue. *)

    val remove_waiter : waiter -> unit
    (** [remove_waiter w] removes a waiter previously added with e.g. [add_waiter].
        If the waiter is already removed, this does nothing. *)
  end
  
  module Switch : sig
    type t = Switch.t

    val add_cancel_hook : t -> (exn -> unit) -> Waiters.waiter
    (** [add_cancel_hook t cancel] registers shutdown function [cancel] with [t].
        When [t] is turned off, [cancel] is called.
        If [t] is already off, it calls [cancel] immediately. *)

    val add_cancel_hook_opt : t option -> (exn -> unit) -> Waiters.waiter
    (**[add_cancel_hook_opt (Some t)] is [add_cancel_hook t].
       If called with [None], it does nothing and returns a dummy waiter. *)
  end

  module Effects : sig
    effect Await : Switch.t option * Ctf.id * 'a Waiters.t -> 'a
    (** Performed when a fibre must be suspended (e.g. because it called {!Promise.await} on an unresolved promise).
        The effect handler should add itself to the list of waiters and block until its callback is invoked.
        The ID is used for tracing. *)

    effect Fork : (unit -> 'a) -> 'a Promise.t
    (** See {!Fibre.fork} *)

    effect Fork_ignore  : (unit -> unit) -> unit
    (** See {!Fibre.fork_ignore} *)

    effect Yield : unit
    (** See {!Fibre.yield} *)
  end
end
