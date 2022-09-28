(** Asynchronous, reliable, domain-safe MPMC messages.

    The mailbox {!Mbox.t} stores a value that can be retrieved by a
    reader via {!Mbox.recv} or pushed by a sender via {!Mbox.send}.
    Readers will block if no value is present and writers will block
    if a value is already present.

    Both sides synchronize via the same internal mutex. There is no thundering
    storm effect on blocked callers.

    Since the buffer for storing values is "1", this is more suited for
    control, and/or other slow messages.

    Example:

    {[
      let mbox = Eio.Mbox.create () in
      Fiber.both
        (fun () -> Eio.Mbox.send mbox "hi")
        (fun () -> traceln "got: %s" (Eio.Mbox.recv mbox))
    ]}
*)

type 'a t

val create : unit -> 'a t
(** [create ()] creates a new mailbox. *)

val send : 'a t -> 'a -> unit
(** [send t m] sends message [m] to mailbox [t].

    If [t] already contains one message, [send] blocks until someone
    retrieves the message via [recv]. [send] is domain-safe and senders
    are blocked in a FIFO queue. *)

val recv : 'a t -> 'a
(** [recv t] retrieves message [m] from mailbox [t].

    If [t] is empty, [recv] blocks until someone calls [send] on
    it. [recv] is domain-safe and multiple readers are blocked in a
    FIFO queue. *)
