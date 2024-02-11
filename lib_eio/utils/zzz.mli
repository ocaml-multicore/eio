(** A set of timers. *)

(** A handle to a registered timer. *)
module Key : sig
  type t
end

type t
(** A set of timers (implemented as a priority queue). *)

type item =
  | Fiber of unit Suspended.t
  | Fn of (unit -> unit)

val create : unit -> t
(** [create ()] is a fresh empty queue. *)

val add : t -> Mtime.t -> item -> Key.t
(** [add t time item] adds a new event, due at [time], and returns its ID.

    If [item] is a {!Fiber},
    you must use {!Eio.Private.Fiber_context.set_cancel_fn} on it before calling {!pop}.
    Your cancel function should call {!remove} (in addition to resuming it). *)

val remove : t -> Key.t -> unit
(** [remove t key] removes an event previously added with [add]. *)

val pop : t -> now:Mtime.t -> [`Due of item | `Wait_until of Mtime.t | `Nothing]
(** [pop ~now t] removes and returns the earliest item due by [now].
    For fibers, it also clears the thread's cancel function.
    If no item is due yet, it returns the time the earliest item becomes due. *)
