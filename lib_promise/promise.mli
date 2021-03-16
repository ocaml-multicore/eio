type 'a t
(** An ['a t] is a promise for a value of type ['a]. *)

type 'a u
(** An ['a u] is a resolver for a promise of type ['a]. *)

val create : unit -> 'a t * 'a u
(** [create ()] is a fresh promise/resolver pair.
    The promise is initially unresolved. *)

val await : 'a t -> 'a
(** [await t] blocks until [t] is resolved.
    If [t] is already resolved then this returns immediately.
    If [t] is broken, it raises the exception. *)

val fulfill : 'a u -> 'a -> unit
(** [fulfill u v] successfully resolves [u]'s promise with the value [v].
    Any threads waiting for the result will be added to the run queue. *)

val break : 'a u -> exn -> unit
(** [break u ex] resolves [u]'s promise with the exception [ex].
    Any threads waiting for the result will be added to the run queue. *)

type 'a waiters

type 'a state =
  | Unresolved of 'a waiters
  | Fulfilled of 'a
  | Broken of exn

val state : 'a t -> 'a state

(** {2 Provider API} *)

val add_waiter : 'a waiters -> (('a, exn) result -> unit) -> unit

effect Await : 'a waiters -> 'a
(** Performed when the user calls [await] on an unresolved promise.
    The handler should add itself to the list of waiters and block until its callback is invoked. *)
