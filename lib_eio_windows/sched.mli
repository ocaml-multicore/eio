(** The scheduler keeps track of all suspended fibers and resumes them as appropriate.

    Each Eio domain has one scheduler, which keeps a queue of runnable
    processes plus a record of all fibers waiting for IO operations to complete. *)

type t

type exit
(** This is equivalent to [unit], but indicates that a function returning this will call {!next}
    and so does not return until the whole event loop is finished. Such functions should normally
    be called in tail position. *)

val with_sched : (t -> 'a) -> 'a
(** [with_sched fn] sets up a scheduler and calls [fn t].
    Typically [fn] will call {!run}.
    When [fn] returns, the scheduler's resources are freed. *)

val run :
    extra_effects:exit Effect.Deep.effect_handler ->
    t -> ('a -> 'b) -> 'a -> 'b [@@alert "-unstable"]
(** [run ~extra_effects t f x] starts an event loop using [t] and runs [f x] as the root fiber within it.

    Unknown effects are passed to [extra_effects]. *)