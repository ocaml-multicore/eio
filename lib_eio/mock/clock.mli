type t = <
  Eio.Time.clock;
  advance : unit;
  set_time : float -> unit;
>

val make : unit -> t
(** [make ()] is a new clock.

    The time is initially set to 0.0 and doesn't change except when you call {!advance} or {!set_time}. *)

val advance : t -> unit
(** [advance t] sets the time to the next scheduled event (adding any due fibers to the run queue).
    @raise Invalid_argument if nothing is scheduled. *)

val set_time : t -> float -> unit
(** [set_time t time] sets the time to [time] (adding any due fibers to the run queue). *)
