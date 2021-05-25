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

  val sub : sw:t -> on_error:(exn -> 'a) -> (t -> 'a) -> 'a
  (** [sub ~sw ~on_error fn] is like [top fn], but the new switch is a child of [sw], so that
      cancelling [sw] also cancels the child (but not the other way around).
      If [fn] raises an exception then it is passed to [on_error].
      If you only want to use [sub] to wait for a group of threads to finish, but not to contain
      errors, you can use [~on_error:raise]. *)

  val check : t -> unit
  (** [check t] checks that [t] is still on.
      @raise Cancelled If the switch is off. *)

  val turn_off : t -> exn -> unit
  (** [turn_off t ex] turns off [t], with reason [ex].
      It returns immediately, without waiting for the shutdown actions to complete.
      If [t] is already off then [ex] is added to the list of exceptions (unless
      [ex] is [Cancelled] or identical to the original exception, in which case
      it is ignored). *)
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

  val fork_sub_ignore : sw:Switch.t -> on_error:(exn -> unit) -> (Switch.t -> unit) -> unit
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

val traceln :
  ?__POS__:string * int * int * int ->
  ('a, Format.formatter, unit, unit) format4 -> 'a
(** [traceln fmt] outputs a debug message (typically to stderr).
    Trace messages are printed by default and do not require logging to be configured first.
    The message is printed with a newline, and is flushed automatically.
    This is intended for quick debugging rather than for production code.
    Examples:
    {[
      traceln "x = %d" x;
      traceln "x = %d" x ~__POSS__;   (* With location information *)
    |}
    @param __POS__ Display [__POS__] as the location of the [traceln] call. *)

(** API for use by the scheduler implementation. *)
module Fibre_impl : sig
  module Waiters : sig
    type 'a t
    (** A queue of callbacks waiting for a value of type ['a]. *)

    type waiter

    val add_waiter : 'a t -> (('a, exn) result -> unit) -> waiter
    (** [add_waiter t fn] adds [fn] to the queue of callbacks to be invoked when the wait is over.
        [fn] will typically add some saved continuation to the runqueue. *)

    val remove_waiter : waiter -> unit
    (** [remove_waiter w] removes a waiter previously added with e.g. [add_waiter].
        If the waiter is already removed, this does nothing. *)
  end
  
  module Switch : sig
    type t = Switch.t

    val with_op : t -> (unit -> 'a) -> 'a
    (** [with_op t fn] prevents [t] from finishing while [fn] is running.
        If [t] is already turned off, [fn] does not run and the exception is raised instead. *)

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

    effect Fork : Switch.t * bool * (unit -> 'a) -> 'a Promise.t
    (** See {!Fibre.fork} *)

    effect Fork_ignore  : Switch.t * (unit -> unit) -> unit
    (** See {!Fibre.fork_ignore} *)

    effect Yield : unit
    (** See {!Fibre.yield} *)
  end
end
