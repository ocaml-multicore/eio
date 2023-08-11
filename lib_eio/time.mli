open Std

type 'a clock_ty = [`Clock of 'a]
type 'a clock_base = 'a r constraint 'a = [> _ clock_ty]

type 'a clock = ([> float clock_ty] as 'a) r

val now : _ clock -> float
(** [now t] is the current time since 00:00:00 GMT, Jan. 1, 1970 - in seconds - according to [t]. *)

val sleep_until : _ clock -> float -> unit
(** [sleep_until t time] waits until the given time is reached. *)

val sleep : _ clock -> float -> unit
(** [sleep t d] waits for [d] seconds. *)

(** Monotonic clocks. *)
module Mono : sig
  (** Monotonic clocks are unaffected by corrections to the real-time clock,
      and so are a better choice for timeouts or measuring intervals,
      where the absolute time doesn't matter.

      A monotonic clock may or may not include time while the computer is suspended. *)

  type ty = Mtime.t clock_ty
  type 'a t = ([> ty] as 'a) r

  val now : _ t -> Mtime.t
  (** [now t] is the current time according to [t]. *)

  val sleep_until : _ t -> Mtime.t -> unit
  (** [sleep_until t time] waits until [time] before returning. *)

  val sleep : _ t -> float -> unit
  (** [sleep t d] waits for [d] seconds. *)

  val sleep_span : _ t -> Mtime.span -> unit
  (** [sleep_span t d] waits for duration [d]. *)
end

(** {2 Timeouts} *)

exception Timeout

val with_timeout : _ clock -> float -> (unit -> ('a, 'e) result) -> ('a, [> `Timeout] as 'e) result
(** [with_timeout clock d fn] runs [fn ()] but cancels it after [d] seconds. *)

val with_timeout_exn : _ clock -> float -> (unit -> 'a) -> 'a
(** [with_timeout_exn clock d fn] runs [fn ()] but cancels it after [d] seconds,
    raising exception {!exception-Timeout}. *)

(** Timeout values. *)
module Timeout : sig
  type t

  val v : _ Mono.t -> Mtime.Span.t -> t
  (** [v clock duration] is a timeout of [duration], as measured by [clock].
      Internally, this is just the tuple [(clock, duration)]. *)

  val seconds : _ Mono.t -> float -> t
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

module Pi : sig
  module type CLOCK = sig
    type t
    type time

    val now : t -> time
    val sleep_until : t -> time -> unit
  end

  type (_, _, _) Resource.pi +=
      Clock : ('t, (module CLOCK with type t = 't and type time = 'time),
         [> 'time clock_ty ]) Resource.pi

  val clock :
    (module CLOCK with type t = 't and type time = 'time) ->
    ('t, [> 'time clock_ty]) Resource.handler
end
