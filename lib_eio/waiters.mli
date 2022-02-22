type 'a t
(* A queue of fibers waiting for something.
   Note: an [_ t] is not thread-safe itself.
   To use share it between domains, the user is responsible for wrapping it in a mutex. *)

val create : unit -> 'a t

val wake_all : 'a t -> 'a -> unit
(** [wake_all t] calls (and removes) all the functions waiting on [t].
    If [t] is shared between domains, the caller must hold the mutex while calling this. *)

val wake_one : 'a t -> 'a -> [`Ok | `Queue_empty]
(** [wake_one t] is like {!wake_all}, but only calls (and removes) the first waiter in the queue.
    If [t] is shared between domains, the caller must hold the mutex while calling this. *)

val is_empty : 'a t -> bool
(** [is_empty t] checks whether there are any functions waiting on [t].
    If [t] is shared between domains, the caller must hold the mutex while calling this,
    and the result is valid until the mutex is released. *)

val await :
  mutex:Mutex.t option ->
  'a t -> Ctf.id -> 'a
(** [await ~mutex t id] suspends the current fiber and adds its continuation to [t].
    When the waiter is woken, the fiber is resumed and returns the result.
    If [t] can be used from multiple domains:
    - [mutex] must be set to the mutex to use to unlock it.
    - [mutex] must be already held when calling this function, which will unlock it before blocking.
    When [await] returns, [mutex] will have been unlocked.
    @raise Cancel.Cancelled if the fiber's context is cancelled *)

val await_internal :
  mutex:Mutex.t option ->
  'a t -> Ctf.id -> Cancel.fiber_context ->
  (('a, exn) result -> unit) -> unit
(** [await_internal ~mutex t id ctx enqueue] is like [await], but the caller has to suspend the fiber.
    This also allows wrapping the [enqueue] function.
    Calls [enqueue (Error (Cancelled _))] if cancelled.
    Note: [enqueue] is called from the triggering domain,
          which is currently calling {!wake_one} or {!wake_all}
          and must therefore be holding [mutex]. *)
