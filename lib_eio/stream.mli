(** Reading from an empty queue will wait until an item is available.
    Writing to a full queue will wait until there is space.

    Example:
    {[
      let t = Stream.create 100 in
      Stream.add t 1;
      Stream.add t 2;
      assert (Stream.take t = 1);
      assert (Stream.take t = 2)
    ]}

    Streams are thread-safe and so can be shared between domains and used
    to communicate between them. *)

type 'a t
(** A queue of items of type ['a]. *)

val create : int -> 'a t
(** [create capacity] is a new stream which can hold up to [capacity] items without blocking writers.

    - If [capacity = 0] then writes block until a reader is ready.
    - If [capacity = 1] then this acts as a "mailbox".
    - If [capacity = max_int] then the stream is effectively unbounded. *)

val add : 'a t -> 'a -> unit
(** [add t item] adds [item] to [t].

    If this would take [t] over capacity, it blocks until there is space. *)

val take : 'a t -> 'a
(** [take t] takes the next item from the head of [t].

    If no items are available, it waits until one becomes available. *)

val take_nonblocking : 'a t -> 'a option
(** [take_nonblocking t] is like [Some (take t)] except that
    it returns [None] if the stream is empty rather than waiting.

    Note that if another domain may add to the stream then a [None]
    result may already be out-of-date by the time this returns. *)

val length : 'a t -> int
(** [length t] returns the number of items currently in [t]. *)

val is_empty : 'a t -> bool
(** [is_empty t] is [length t = 0]. *)
