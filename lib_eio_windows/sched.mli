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

val next : t -> exit
(** [next t] asks the scheduler to transfer control to the next runnable fiber,
    or wait for an event from the OS if there is none. This should normally be
    called in tail position from an effect handler. *)

val await_readable : t -> unit Eio_utils.Suspended.t -> Unix.file_descr -> exit
(** [await_readable t k fd] arranges for [k] to be resumed when [fd] is ready for reading. *)

val await_writable : t -> unit Eio_utils.Suspended.t -> Unix.file_descr -> exit
(** [await_readable t k fd] arranges for [k] to be resumed when [fd] is ready for writing. *)

val await_timeout : t -> unit Eio_utils.Suspended.t -> Mtime.t -> exit
(** [await_timeout t k time] adds [time, k] to the timer.

    When [time] is reached, [k] is resumed. Cancelling [k] removes the entry from the timer. *)

val enter : (t -> 'a Eio_utils.Suspended.t -> exit) -> 'a
(** [enter fn] suspends the current fiber and runs [fn t k] in the scheduler's context.

    [fn] should either resume [k] immediately itself, or call one of the [await_*] functions above. *)
