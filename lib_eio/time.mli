class virtual clock : object
  method virtual now : float
  method virtual sleep_until : float -> unit
end

val now : #clock -> float
(** [now t] is the current time according to [t]. *)

val sleep_until : #clock -> float -> unit
(** [sleep_until t time] waits until the given time is reached. *)

val sleep : #clock -> float -> unit
(** [sleep t d] waits for [d] seconds. *)

(** {2 Timeouts} *)

exception Timeout

val with_timeout : #clock -> float -> (unit -> ('a, 'e) result) -> ('a, [> `Timeout] as 'e) result
(** [with_timeout clock d fn] runs [fn ()] but cancels it after [d] seconds. *)

val with_timeout_exn : #clock -> float -> (unit -> 'a) -> 'a
(** [with_timeout_exn clock d fn] runs [fn ()] but cancels it after [d] seconds,
    raising exception {!exception-Timeout}. *)

(** Timeout values. *)
module Timeout : sig
  type t

  val of_s : #clock -> float -> t
  (** [of_s clock duration] is a timeout of [duration] seconds, as measured by [clock].
      Internally, this is just the tuple [(clock, duration)]. *)

  val none : t
  (** [none] is an infinite timeout. *)

  val run : t -> (unit -> ('a, 'e) result) -> ('a, [> `Timeout] as 'e) result
  (** [run t fn] runs [fn ()] but cancels it if it takes longer than allowed by timeout [t]. *)

  val run_exn : t -> (unit -> 'a) -> 'a
  (** [run_exn t fn] runs [fn ()] but cancels it if it takes longer than allowed by timeout [t],
      raising exception {!exception-Timeout}. *)

  val pp : t Fmt.t
  (** [pp] formats a timeout as a duration (e.g. "5s").
      This is intended for use in error messages and logging and is rounded. *)
end
