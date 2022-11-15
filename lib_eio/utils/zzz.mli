(** A set of timers. *)

(** A handle to a registered timer. *)
module Key : sig
  type t
end

type t
(** A set of timers (implemented as a priority queue). *)

val create : unit -> t
(** [create ()] is a fresh empty queue. *)

val add : t -> Mtime.t -> unit Suspended.t -> Key.t
(** [add t time thread] adds a new event, due at [time], and returns its ID.
    You must use {!Eio.Private.Fiber_context.set_cancel_fn} on [thread] before
    calling {!pop}.
    Your cancel function should call {!remove} (in addition to resuming [thread]). *)

val remove : t -> Key.t -> unit
(** [remove t key] removes an event previously added with [add]. *)

val pop : t -> now:Mtime.t -> [`Due of unit Suspended.t | `Wait_until of Mtime.t | `Nothing]
(** [pop ~now t] removes and returns the earliest thread due by [now].
    It also clears the thread's cancel function.
    If no thread is due yet, it returns the time the earliest thread becomes due. *)
