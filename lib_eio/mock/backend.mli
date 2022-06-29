(** A dummy Eio backend with no actual IO.

    This backend does not support the use of multiple domains or systhreads,
    but the tradeoff is that it can reliably detect deadlock, because if the
    run queue is empty then it knows that no wake up event can be coming from
    elsewhere. *)

exception Deadlock_detected

val run : (unit -> 'a) -> 'a
(** [run fn] runs an event loop and then calls [fn env] within it.
    @raise Deadlock_detected if the run queue becomes empty but [fn] hasn't returned. *)
