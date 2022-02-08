(** A lock-free multi-producer, single-consumer, thread-safe queue without support for cancellation.
    This makes a good data structure for a scheduler's run queue. *)

type 'a t
(** A queue of items of type ['a]. *)

exception Closed

val create : unit -> 'a t
(** [create ()] is a new empty queue. *)

val push : 'a t -> 'a -> unit
(** [push t x] adds [x] to the tail of the queue.
    This can be used safely by multiple producer domains, in parallel with the other operations.
    @raise Closed if [t] is closed. *)

val push_head : 'a t -> 'a -> unit
(** [push_head t x] inserts [x] at the head of the queue.
    This can only be used by the consumer (if run in parallel with {!pop}, the item might be skipped).
    @raise Closed if [t] is closed and empty. *)

val pop : 'a t -> 'a option
(** [pop t] removes the head item from [t] and returns it.
    Returns [None] if [t] is currently empty.
    @raise Closed if [t] has been closed and is empty. *)

val is_empty : 'a t -> bool
(** [is_empty t] is [true] if calling [pop] would return [None].
    @raise Closed if [t] has been closed and is empty. *)

val close : 'a t -> unit
(** [close t] marks [t] as closed, preventing any further items from being pushed. *)
