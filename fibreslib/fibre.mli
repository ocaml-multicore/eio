val fork : (unit -> 'a) -> 'a Promise.t
(** [fork fn] starts running [fn ()] and returns a promise for its result. *)

val fork_detach : (unit -> unit) -> on_error:(exn -> unit) -> unit
(** [fork_detach fn ~on_error] runs [fn ()] in a new fibre, but does not
    wait for it to finish.
    If the fibre raises an exception, [on_error] is called to handle it.
    [on_error] must not itself raise an exception. *)

val yield : unit -> unit
(** [yield ()] asks the scheduler to switch to the next runnable task.
    The current task remains runnable, but goes to the back of the queue. *)

(** {2 Provider API} *)

effect Fork  : (unit -> 'a) -> 'a Promise.t
effect Fork_detach  : (unit -> unit) * (exn -> unit) -> unit
effect Yield : unit
