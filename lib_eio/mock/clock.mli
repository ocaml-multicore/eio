module type S = sig
  type time

  type t = <
    time Eio.Time.clock_base;
    advance : unit;
    set_time : time -> unit;
  >

  val make : unit -> t
  (** [make ()] is a new clock.

      The time is initially set to 0.0 and doesn't change except when you call {!advance} or {!set_time}. *)

  val advance : t -> unit
  (** [advance t] sets the time to the next scheduled event (adding any due fibers to the run queue).
      @raise Invalid_argument if nothing is scheduled. *)

  val set_time : t -> time -> unit
  (** [set_time t time] sets the time to [time] (adding any due fibers to the run queue). *)
end

include S with type time := float

module Mono : S with type time := Mtime.t
