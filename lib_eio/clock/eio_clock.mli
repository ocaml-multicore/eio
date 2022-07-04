class virtual t : object
  method virtual now : float 
  method virtual now_ns : int64
  method virtual sleep_until: float -> unit
end 

val system_clock: unit -> int64
(** [system_clock ()] is the current system clock time in nanoseconds. *)

val mono_clock: unit -> int64
(** [mono_clock ()] is the current monotonic clock time in nanoseconds. *)

val ns_to_seconds: int64 -> float
(** [ns_to_seconds ns] converts nanoseconds time [ns] to seconds. *)
