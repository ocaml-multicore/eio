(** A lock-free queue-like structure with suspension and cancellation.

    This module provides an infinite sequence of atomic cells, which can be used for whatever you like.
    There are two pointers into this sequence: a suspend (consumer) pointer and a resume (producer) pointer.
    These are similar to the head and tail pointers in a traditional queue,
    except that the consumer is also permitted to get ahead of the producer.

    To use this as a plain queue, each producer calls {!Make.next_resume} to get the
    cell at the resume (tail) pointer (and advance it atomically), then stores
    its value in the cell. Each consumer calls {!Make.next_suspend} to get the next
    cell at the head of the queue (and advance the suspend pointer).

    The consumer/suspender is permitted to get ahead of the producer. In this
    case, the consumer will CAS the cell from its initial state to a Request
    state containing a callback to receive the value when it arrives. When a
    producer later tries to CAS the cell from the initial state to holding a
    value, it will fail and find the Request with the callback function
    instead. It can then provide the value directly to the callback.

    A suspender can be cancelled by CASing the Request to a Cancelled state.
    It should also call {!Make.cancel_cell} (if the CAS succeeds), to allow the cell to be freed.
    If a resumer's CAS fails because the cell is cancelled, it can retry with a fresh cell.

    For efficiency, cells are grouped into segments, which are stored in a linked list.
    Once all the cells in a segment are cancelled, the whole segment may be freed.

    This is based on {{:https://arxiv.org/pdf/2111.12682.pdf}A formally-verified
    framework for fair synchronization in kotlin coroutines, Appendix C},
    which contains more details and examples of use.

    This module also adds the {!Make.resume_all} function, which is useful for broadcasting.
*)

(** The signature for user-defined cell contents. *)
module type CELL = sig
  type 'a t

  val init : 'a t
  (** The value to give newly-allocated cells. *)

  val segment_order : int
  (** The number of bits to use for the offset into the segment.

      The number of cells per segment is [2 ** segment_order]. *)

  val dump : _ t Fmt.t
  (** Display the cell state for debugging. *)
end

module Make(Cell : CELL) : sig
  type 'a t

  type 'a segment

  val make : unit -> 'a t
  (** [make ()] is a fresh sequence of cells. *)

  val next_suspend : 'a t -> 'a segment * 'a Cell.t Atomic.t
  (** [next_suspend t] atomically returns the next suspend cell and its segment.

      If multiple domains call this at the same time, they will each get a different location.

      The cell might or might not have already been filled in by a resumer.
      You need to handle both cases (typically by using {!Atomic.compare_and_set}).

      The segment can be used with {!cancel_cell}.

      This function is lock-free and is safe to call even from a signal handler or GC finalizer. *)

  val next_resume : 'a t -> 'a Cell.t Atomic.t
  (** [next_resume t] atomically returns the next resume cell.

      If multiple domains call this at the same time, they will each get a different cell.

      The cell might or might not contain a request from a suspender that got there first.
      You need to handle both cases (typically by using {!Atomic.compare_and_set}).

      Note: cancelled cells may or may not be skipped (you need to handle the case of the
      cell you get being cancelled before you can write to it, but you also
      can't rely on seeing every cancelled cell, as cancelled segments may be deleted).

      This function is lock-free and is safe to call even from a signal handler or GC finalizer. *)

  val resume_all : 'a t -> ('a Cell.t Atomic.t -> unit) -> unit
  (** [resume_all t f] advances the resume position to the current suspend position,
      then calls [f cell] on each cell advanced over.

      Note: as with {!next_resume}, [f] may be called for some cancelled cells but not others.

      [f] must not raise an exception (if it does, it will not be called on the remaining cells).

      If the resume position is ahead of the suspend position, then calling this function does nothing.

      This function is lock-free and is safe to call even from a signal handler or GC finalizer. *)

  val cancel_cell : 'a segment -> unit
  (** [cancel_cell segment] increments the segment's count of the number of cancelled cells.

      Once all cells are cancelled it may be possible to discard the whole segment.
      This avoids leaking memory if a user keeps suspending and then cancelling.

      You must not call this more than once per cell.

      This function is lock-free and is safe to call even from a signal handler or GC finalizer. *)

  val validate : _ t -> unit
  (** [validate t] checks that [t] is in a valid state, assuming there are no operations currently in progress. *)

  val dump : _ t Fmt.t
  (** [dump] outputs the internal state of a [_ t], for debugging. *)
end
