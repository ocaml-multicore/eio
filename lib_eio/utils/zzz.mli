(** A set of timers. *)

(** A handle to a registered timer. *)
module Key : sig
  type t
end

module type SUSPENDED = sig
  type 'a t

  val fiber : 'a t -> Eio.Private.Fiber_context.t
end

module type S = sig
  type t
  (** A set of timers (implemented as a priority queue). *)

  type suspended
  (** Represents a suspended computation. *)

  type timer_state = [`Due of suspended | `Wait_until of Eio.Time.t | `Nothing]

  val create : Eio.Time.clock -> t
  (** [create ()] is a fresh empty queue. *)

  val add : t -> Eio.Time.t -> suspended -> Key.t
  (** [add t clock time thread] adds a new event, due at [time], and returns its ID.
      You must use {!Eio.Private.Fiber_context.set_cancel_fn} on [thread] before
      calling {!pop}.
      Your cancel function should call {!remove} (in addition to resuming [thread]). *)

  val remove : t -> Key.t -> unit
  (** [remove t key] removes an event previously added with [add]. *)

  val pop : t -> timer_state
  (** [pop t] removes and returns the earliest thread due by [now].
      It also clears the thread's cancel function.
      If no thread is due yet, it returns the time the earliest thread becomes due. *)
end

module Make (Suspended: SUSPENDED) : S
  with type suspended = unit Suspended.t

module Make_expired (Zzz : S) : sig

  val expired_timer : Zzz.t list -> Zzz.timer_state
  (** [expired_timer sleep_timers] iterates through [sleep_timers] and returns
      the first available [`Due _] timer. If none of the timers has an expired timer,
      then the last timer which has [`Wait_until] timer is returned. If all of the
      timers are empty then [`Nothing] is returned. *)
end
