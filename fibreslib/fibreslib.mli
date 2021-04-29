module Promise : sig
  type 'a t
  (** An ['a t] is a promise for a value of type ['a]. *)

  type 'a u
  (** An ['a u] is a resolver for a promise of type ['a]. *)

  val create : ?label:string -> unit -> 'a t * 'a u
  (** [create ()] is a fresh promise/resolver pair.
      The promise is initially unresolved. *)

  val await : 'a t -> 'a
  (** [await t] blocks until [t] is resolved.
      If [t] is already resolved then this returns immediately.
      If [t] is broken, it raises the exception. *)

  val await_result : 'a t -> ('a, exn) result
  (** [await_result t] is like [await t], but returns [Error ex] if [t] is broken
      instead of raising an exception. *)

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
  val fork : (unit -> 'a) -> 'a Promise.t
  (** [fork fn] starts running [fn ()] in a new fibre and returns a promise for its result. *)

  val fork_detach : (unit -> unit) -> on_error:(exn -> unit) -> unit
  (** [fork_detach fn ~on_error] runs [fn ()] in a new fibre, but does not
      wait for it to finish.
      If the fibre raises an exception, [on_error] is called to handle it.
      [on_error] must not itself raise an exception. *)

  val yield : unit -> unit
  (** [yield ()] asks the scheduler to switch to the next runnable task.
      The current task remains runnable, but goes to the back of the queue. *)
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

(** API for use by the scheduler implementation. *)
module Fibre_impl : sig
  module Waiters : sig
    type 'a t
    (** A queue of callbacks waiting for a value of type ['a]. *)

    val add_waiter : 'a t -> (('a, exn) result -> unit) -> unit
    (** [add_waiter t fn] adds [fn] to the queue of callbacks to be invoked when the wait is over.
        [fn] will typically add some saved continuation to the runqueue. *)
  end

  module Effects : sig
    effect Await : Ctf.id * 'a Waiters.t -> 'a
    (** Performed when a fibre must be suspended (e.g. because it called {!Promise.await} on an unresolved promise).
        The effect handler should add itself to the list of waiters and block until its callback is invoked.
        The ID is used for tracing. *)

    effect Fork  : (unit -> 'a) -> 'a Promise.t
    (** See {!Fibre.fork} *)

    effect Fork_detach  : (unit -> unit) * (exn -> unit) -> unit
    (** See {!Fibre.fork_detach} *)

    effect Yield : unit
    (** See {!Fibre.yield} *)
  end
end
