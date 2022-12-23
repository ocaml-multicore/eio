class virtual ['a] clock_base : object
  method virtual now : 'a
  method virtual sleep_until : 'a -> unit
end

class virtual clock : object
  inherit [float] clock_base
end

val now : #clock -> float
(** [now t] is the current time since 00:00:00 GMT, Jan. 1, 1970 - in seconds - according to [t]. *)

val sleep_until : #clock -> float -> unit
(** [sleep_until t time] waits until the given time is reached. *)

val sleep : #clock -> float -> unit
(** [sleep t d] waits for [d] seconds. *)

(** Monotonic clocks. *)
module Mono : sig
  (** Monotonic clocks are unaffected by corrections to the real-time clock,
      and so are a better choice for timeouts or measuring intervals,
      where the absolute time doesn't matter.

      A monotonic clock may or may not include time while the computer is suspended. *)

  class virtual t : object
    inherit [Mtime.t] clock_base
  end

  val now : #t -> Mtime.t
  (** [now t] is the current time according to [t]. *)

  val sleep_until : #t -> Mtime.t -> unit
  (** [sleep_until t time] waits until [time] before returning. *)

  val sleep : #t -> float -> unit
  (** [sleep t d] waits for [d] seconds. *)

  val sleep_span : #t -> Mtime.span -> unit
  (** [sleep_span t d] waits for duration [d]. *)
end

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
  [@@deprecated "Use [seconds] instead, with a monotonic clock"]

  val v : #Mono.t -> Mtime.Span.t -> t
  (** [v clock duration] is a timeout of [duration], as measured by [clock].
      Internally, this is just the tuple [(clock, duration)]. *)

  val seconds : #Mono.t -> float -> t
  (** [seconds clock duration] is a timeout of [duration] seconds, as measured by [clock]. *)

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
