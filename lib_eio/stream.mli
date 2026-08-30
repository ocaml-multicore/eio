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

type drop_priority = Newest | Oldest

val add_nonblocking : drop_priority: drop_priority -> 'a t -> 'a -> 'a option
(** [add_nonblocking ~drop_priority t item] is like [(add t item); None] except that
    it returns [Some dropped_item] if the stream is full rather than waiting, where
    [dropped_item] is [item] if [drop_priority = Newest], and the first element of the
    stream if [drop_priority = Oldest].
        
    In other words, if the stream is full then:
    - [add_nonblocking ~drop_priority:Newest t item] is like [Some item]; and
    - [add_nonblocking ~drop_priority:Oldest t item] is like
      [let dropped_item = take t in add t item; Some dropped_item]
      except that no other stream operation can happen (even in other threads)
      between the [take] and the [add].
    
    On streams of capacity [0], this always returns [Some item], even if a reader is waiting. *)

val take_nonblocking : 'a t -> 'a option
(** [take_nonblocking t] is like [Some (take t)] except that
    it returns [None] if the stream is empty rather than waiting.

    Note that if another domain may add to the stream then a [None]
    result may already be out-of-date by the time this returns. *)

val length : 'a t -> int
(** [length t] returns the number of items currently in [t]. *)

val capacity : 'a t -> int
(** [capacity t] returns the number of items [t] can hold without blocking writers. *)

val is_empty : 'a t -> bool
(** [is_empty t] is [length t = 0]. *)

val is_full : 'a t -> bool
(** [is_full t] is [length t = capacity t]. *)

val dump : 'a t Fmt.t
(** For debugging. *)
