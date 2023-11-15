(** A dummy Eio backend with no actual IO.

    This backend does not support the use of multiple domains or systhreads,
    but the tradeoff is that it can reliably detect deadlock, because if the
    run queue is empty then it knows that no wake up event can be coming from
    elsewhere. *)

exception Deadlock_detected

val run : (unit -> 'a) -> 'a
(** [run fn] runs an event loop and then calls [fn env] within it.
    @raise Deadlock_detected if the run queue becomes empty but [fn] hasn't returned. *)

type stdenv = <
  clock : Clock.t;
  mono_clock : Clock.Mono.t;
  debug : Eio.Debug.t;
  backend_id: string;
>

val run_full : (stdenv -> 'a) -> 'a
(** [run_full] is like {!run} but also provides a mock environment.

    The mock monotonic clock it provides advances automatically when there is nothing left to do.
    The mock wall clock is linked directly to the monotonic time. *)
