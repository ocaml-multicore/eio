type 'a t

val create : unit -> 'a t
val add_waiter_protected : mutex:Mutex.t -> 'a t -> (('a, exn) result -> unit) -> Hook.t
val add_waiter : 'a t -> (('a, exn) result -> unit) -> Hook.t
val wake_all : 'a t -> ('a, exn) result -> unit
val wake_one : 'a t -> ('a, exn) result -> [`Ok | `Queue_empty]
val is_empty : 'a t -> bool

val await :
  mutex:Mutex.t option ->
  'a t -> Ctf.id -> ('a, exn) result
(** [await ~mutex t id] suspends the current fibre and adds its continuation to [t].
    When the waiter is woken, the fibre is resumed and returns the result.
   If [t] can be used from multiple domains,
   [mutex] must be set to the mutex to use to unlock it.
   [mutex] should be already held when calling this function,
   which will unlock it before blocking. *)

val await_internal :
  mutex:Mutex.t option ->
  'a t -> Ctf.id -> Cancel.fibre_context ->
  ((('a, exn) result, exn) result -> unit) -> unit
(** [await_internal] is like [await], but the caller has to suspend the fibre.
    This also allows wrapping the enqueue funciton. *)
