(** Allows a single fiber to wait to be notified by another fiber in the same domain.
    If multiple fibers need to wait at once, or the notification comes from another domain,
    this can't be used. *)

type 'a t
(** A handle representing a fiber that might be sleeping.
    It is either in the Running or Sleeping state. *)

val create : unit -> 'a t
(** [create ()] is a new waiter, initially in the Running state. *)

val wake : 'a t -> ('a, exn) result -> bool
(** [wake t v] resumes [t]'s fiber with value [v] and returns [true] if it was sleeping.
    If [t] is Running then this just returns [false]. *)

val wake_if_sleeping : unit t -> unit
(** [wake_if_sleeping] is [ignore (wake t (Ok ()))]. *)

val await : 'a t -> string -> Trace.id -> 'a
(** [await t op id] suspends the calling fiber, changing [t]'s state to Sleeping.
    If the fiber is cancelled, a cancel exception is raised.
    [op] and [id] are used for tracing. *)

val await_protect : 'a t -> string -> Trace.id -> 'a
(** [await_protect] is like {!await}, but the sleep cannot be cancelled. *)
