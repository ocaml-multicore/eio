(** Each Windows event loop runs fibers, timers and a completion port on one domain. *)

val run : extra_effects:unit Effect.Deep.effect_handler -> ('a -> 'b) -> 'a -> 'b
(** [run ~extra_effects f x] runs [f x] as the root fiber of a new scheduler.
    [extra_effects] can perform scheduler effects in the calling fiber. *)

val sleep_until : Mtime.t -> unit
(** Suspend until the given monotonic time. Cancellation removes the timer. *)
