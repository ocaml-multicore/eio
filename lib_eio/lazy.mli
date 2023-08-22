(** This is like [Stdlib.Lazy], but multiple fibers or domains can force at once. *)

type 'a t
(** A lazy value that produces a value of type ['a]. *)

val from_fun :
  cancel:[`Restart | `Record | `Protect] ->
  (unit -> 'a) -> 'a t
(** [from_fun ~cancel fn] is a lazy value that runs [fn ()] the first time it is forced.

    [cancel] determines how cancellation is handled while forcing:

    - [`Restart] : if the forcing fiber is cancelled, the next waiting fiber runs [fn] again.
    - [`Record] : the failure is recorded and the lazy value will always report cancelled if used.
    - [`Protect] : the forcing fiber is protected from cancellation while running. *)

val from_val : 'a -> 'a t
(** [from_val v] is a lazy value that is already forced.

    It is equivalent to [from_fun (fun () -> v)]. *)

val force : 'a t -> 'a
(** [force t] returns the result of running the function passed to {!from_fun}.

    If the function is currently running, this waits for it to finish and then retries.
    If the function has already completed then it returns the saved result.
    If the function returned an exception then [force] re-raises it. *)
