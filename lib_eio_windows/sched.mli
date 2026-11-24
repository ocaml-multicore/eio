(** Each Windows event loop runs fibers, timers and a completion port on one domain. *)

type exit
(** The result of returning control to the scheduler. *)

val run : extra_effects:exit Effect.Deep.effect_handler -> ('a -> 'b) -> 'a -> 'b
(** [run ~extra_effects f x] runs [f x] as the root fiber of a new scheduler.
    Unhandled effects are passed to [extra_effects]. *)

val sleep_until : Mtime.t -> unit
(** Suspend until the given monotonic time. Cancellation removes the timer. *)
