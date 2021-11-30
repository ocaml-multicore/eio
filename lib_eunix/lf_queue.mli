(** A lock-free multi-producer, single-consumer, thread-safe queue without support for cancellation.
    This makes a good data structure for a scheduler's run queue. *)

type 'a t
(** A queue of items of type ['a]. *)

val create : unit -> 'a t

val push : 'a t -> 'a -> unit

val pop : 'a t -> 'a option

val is_empty : 'a t -> bool
