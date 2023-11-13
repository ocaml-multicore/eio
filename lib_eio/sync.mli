(* A lock-free synchronous channel with cancellation, using Cells.

   Producers and consumers are paired off and then the producer transfers its
   value to the consumer. This is effectively a bounded queue with a capacity
   of zero.

   Both producers and consumers can cancel while waiting. *)

type 'a t
(** A lock-free synchronous channel. *)

val create : unit -> 'a t
(** [create ()] is a fresh channel with a balance of 0. *)

val put : 'a t -> 'a -> unit
(** [put t x] gives [x] to a waiting consumer.

    If no consumer is available, it waits until one comes along and accepts [x].

    Note: Producers are mostly handled fairly, in the order in which they arrive,
    but consumers can cancel or reject values so this isn't guaranteed.

    @raise Invalid_argument if [t] was closed before [x] was added. *)

val take : 'a t -> ('a, [> `Closed]) result
(** [take t] waits until a producer is available with an item and then returns it.

    Note: Consumers are mostly handled fairly, in the order in which they arrive,
    but producers can cancel so this isn't guaranteed if [t] is shared between
    domains.

    Returns [Error `Closed] if [t] was closed before an item was taken. *)

val take_nonblocking : 'a t -> ('a, [> `Would_block | `Closed]) result
(** [take_nonblocking t] is like {!take}, but returns [Error `Would_block] if no producer is immediately available.

    Note: When [t] is shared between domains, it is possible that a producer may be assigned but still be
    in the process of writing its value to [t]. In this case, [take_nonblocking] will cancel it,
    causing the old producer to lose its place in the queue and have to rejoin at the end.
    Since the producer reached the head of the queue while it was still joining,
    the queue is presumably very short in this case anyway. *)

val close : 'a t -> unit
(** [close t] prevents any further items from being added to [t].

    Any consumers or producers that were waiting will receive an exception.
    If [t] is already closed then this does nothing. *)

val balance : 'a t -> (int, [> `Closed]) result
(** [balance t] is the number of waiting producers minus the number of waiting consumers.

    If the balance is non-negative then it is the number of waiting producers.
    If non-positive, it is the number of waiting consumers.
    There cannot be waiting producers and waiting consumers at the same time.

    If [t] is shared between domains then the value may already be out-of-date
    by the time this function returns, so this is mostly useful for debugging
    or reporting metrics. *)

val dump : 'a t Fmt.t
(** [dump] formats the internal state of a channel, for testing and debugging. *)
