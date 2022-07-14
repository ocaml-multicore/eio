(** A set of timers. *)

(** A handle to a registered timer. *)
module Key : sig
  type t
end

module type SUSPENDED = sig
  type 'a t

  val fiber : 'a t -> Eio.Private.Fiber_context.t
end

module Make (Suspended: SUSPENDED) : sig
  type t
  (** A set of timers (implemented as a priority queue). *)

  val create : Eio.Time.clock -> t
  (** [create ()] is a fresh empty queue. *)

  val add : t -> int64 -> unit Suspended.t -> Key.t
  (** [add t clock time thread] adds a new event, due at [time], and returns its ID.
      You must use {!Eio.Private.Fiber_context.set_cancel_fn} on [thread] before
      calling {!pop}.
      Your cancel function should call {!remove} (in addition to resuming [thread]). *)

  val remove : t -> Key.t -> unit
  (** [remove t key] removes an event previously added with [add]. *)

  val pop : t -> [`Due of unit Suspended.t | `Wait_until of int64 | `Nothing]
  (** [pop t] removes and returns the earliest thread due by [now].
      It also clears the thread's cancel function.
      If no thread is due yet, it returns the time the earliest thread becomes due. *)
end
