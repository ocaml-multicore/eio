(** A mutex can be used to ensure that only one piece of code can access a shared resource at one time.

    Unlike {!Stdlib.Mutex}, which blocks the whole domain while waiting to take the mutex,
    this module allows other Eio fibers to run while waiting.
    You should use this module if your critical section may perform blocking operations,
    while [Stdlib.Mutex] may be more efficient if the lock is held only briefly and
    the critial section does not switch fibers.

    Note that mutexes are often unnecessary for code running in a single domain, as
    the scheduler will only switch to another fiber if you perform an operation that
    can block. *)

type t
(** The type for a concurrency-friendly mutex. *)

exception Poisoned of exn
(** Raised if you attempt to use a mutex that has been disabled. *)

val create : unit -> t
(** [create ()] creates an initially unlocked mutex. *)

val use_rw : protect:bool -> t -> (unit -> 'a) -> 'a
(** [use_rw ~protect t fn] waits for the mutex to be free and then executes [fn ()] while holding the mutex locked.
    [fn] may mutate the resource protected by the mutex,
    but must ensure the resource is in a consistent state before returning.
    If [fn] raises an exception, the mutex is disabled and cannot be used again.
    @param protect If [true], uses {!Cancel.protect} to prevent the critical section from being cancelled.
                   Cancellation is not prevented while waiting to take the lock. *)

val use_ro : t -> (unit -> 'a) -> 'a
(** [use_ro t fn] is like [use_rw ~protect:false],
    but if [fn] raises an exception it unlocks the mutex instead of disabling it.
    Use this if you only need read-only access to the mutex's resource and so
    know that it will be in a consistent state even if an exception is raised. *)

(** {2 Low-level API}

    Care must be taken when locking a mutex manually. It is easy to forget to unlock it in some cases,
    which will result in deadlock the next time a fiber tries to use it.
    In particular, you need to consider:

    - What happens if your critical section raises an exception.
    - What happens if your fiber is cancelled while in its critical section.
 *)

val lock : t -> unit
(** Lock the given mutex. Only one fiber can have the mutex locked at any time.
    A fiber that attempts to lock a mutex already locked by another fiber
    will suspend until the other fiber unlocks the mutex.
    If no other fiber has the lock, this returns immediately without switching fibers. *)

val unlock : t -> unit
(** [unlock t] unlocks the mutex.
    @raises Sys_error if the mutex is unlocked. *)

val try_lock : t -> bool
(** Same as {!lock}, but does not suspend the calling thread if the mutex is already locked:
    just return [false] immediately in that case. If the mutex is unlocked, lock it and return [true]. *)
