(** A condition variable *)

type t
(** Condition variables to synchronize between fibers. *)

val create : unit -> t
(** [create ()] creates a new condition variable *)

val await : ?mutex:Eio_mutex.t -> t -> unit
(** [await ~mutex cond] pauses the current fiber until it is notified by [cond]. 
    If [mutex] is set, it is unlocked while the fiber is waiting and locked when 
    it is woken up. *)

val broadcast : t -> unit
(** [broadcast cond] wakes up waiting fibers. 
    If no fibers are waiting, nothing happens and the function returns. *)
