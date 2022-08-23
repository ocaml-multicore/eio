class virtual ['a] clock : object
  method virtual now : 'a
  method virtual sleep_until : 'a -> unit
  method virtual add_seconds : 'a -> float -> 'a
  method virtual to_seconds : 'a -> float
end

val now : 'a #clock -> 'a
(** [now t] is the current time according to [t]. *)

val sleep_until : 'a #clock -> 'a -> unit
(** [sleep_until t time] waits until the given time is reached. *)

val sleep : 'a #clock -> float -> unit
(** [sleep t d] waits for [d] seconds. *)

val to_seconds : 'a #clock -> 'a -> float
(** [to_seconds clock time] converts [time] to fractional seconds using [clock]. *)

val with_timeout : 'a #clock -> float -> (unit -> ('a, 'e) result) -> ('a, [> `Timeout] as 'e) result
(** [with_timeout clock d fn] runs [fn ()] but cancels it after [d] seconds. *)

exception Timeout

val with_timeout_exn : 'a #clock -> float -> (unit -> 'a) -> 'a
(** [with_timeout_exn clock d fn] runs [fn ()] but cancels it after [d] seconds, 
    raising exception {!exception-Timeout}. *)

(** Timeout values. *)
module Timeout : sig
  type 'a t

  val of_s : 'a #clock -> float -> 'a t
  (** [of_s clock duration] is a timeout of [duration] seconds, as measured by [clock].
      Internally, this is just the tuple [(clock, duration)]. *)

  val none : 'a t
  (** [none] is an infinite timeout. *)

  val run : 'a t -> (unit -> ('b, 'e) result) -> ('b, [> `Timeout] as 'e) result
  (** [run t fn] runs [fn ()] but cancels it if it takes longer than allowed by timeout [t]. *)

  val run_exn : 'a t -> (unit -> 'b) -> 'b
  (** [run_exn t fn] runs [fn ()] but cancels it if it takes longer than allowed by timeout [t],
      raising exception {!exception-Timeout}. *)

  val pp : 'a t Fmt.t
  (** [pp] formats a timeout as a duration (e.g. "5s").
      This is intended for use in error messages and logging and is rounded. *)
end
