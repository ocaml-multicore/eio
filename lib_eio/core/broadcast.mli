(** A lock-free queue of waiters that should all be resumed at once.

    This uses {!Cells} internally. *)

type t

type request
(** A handle to a pending request that can be used to cancel it. *)

val create : unit -> t
(** [create ()] is a fresh broadcast queue. *)

val suspend : t -> (unit -> unit) -> request option
(** [suspend t fn] arranges for [fn ()] to be called on {!resume_all}.

    [fn ()] may be called from the caller's context, or by [resume_all],
    so it needs to be able to cope with running in any context where that
    can run. For example, [fn] must be safe to call from a signal handler
    if [resume_all] can be called from one. [fn] must not raise.

    The returned request can be used to cancel. It can be [None] in the
    (unlikely) event that [t] got resumed before the function returned. *)

val resume_all : t -> unit
(** [resume_all t] calls all non-cancelled callbacks attached to [t],
    in the order in which they were suspended.

    This function is lock-free and can be used safely even from a signal handler or GC finalizer. *)

val cancel : request -> bool
(** [cancel request] attempts to remove a pending request.

    It returns [true] if the request was cancelled, or [false] if it got
    resumed before that could happen. *)

val dump : Format.formatter -> t -> unit
(** Display the internal state of a queue, for debugging. *)
