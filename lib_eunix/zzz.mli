module Key : sig
  type t
end

type t
(** A queue of scheduled events. *)

val create : unit -> t
(** [create ()] is a fresh empty queue. *)

val add : cancel_hook:Eio.Hook.t ref -> t -> float -> unit Suspended.t -> Key.t
(** [add ~cancel_hook t time thread] adds a new event, due at [time], and returns its ID.
    [cancel_hook] will be released when the event is later returned by {!pop}. *)

val remove : t -> Key.t -> unit
(** [remove t key] removes an event previously added with [add]. *)

val pop : t -> now:float -> [`Due of unit Suspended.t | `Wait_until of float | `Nothing]
(** [pop ~now t] removes and returns the earliest thread due by [now].
    If no thread is due yet, it returns the time the earliest thread becomes due. *)
