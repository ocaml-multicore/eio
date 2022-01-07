module Key : sig
  type t
end

type t
(** A queue of scheduled events. *)

val create : unit -> t
(** [create ()] is a fresh empty queue. *)

val add : t -> float -> unit Suspended.t -> Key.t
(** [add t time thread] adds a new event, due at [time], and returns its ID.
    You must use {!Eio.Private.Fibre_context.set_cancel_fn} on [thread] before
    calling {!pop}. *)

val remove : t -> Key.t -> unit
(** [remove t key] removes an event previously added with [add]. *)

val pop : t -> now:float -> [`Due of unit Suspended.t | `Wait_until of float | `Nothing]
(** [pop ~now t] removes and returns the earliest thread due by [now].
    It also clears the thread's cancel function.
    If no thread is due yet, it returns the time the earliest thread becomes due. *)
