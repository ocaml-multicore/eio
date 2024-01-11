(** {1 Eio scheduler setup} *)

val start : (unit -> unit) -> unit
(** [start f] executes function [f] asynchronously in a context where
    Eio operations can be performed.

    This function is an alias for {!Eio_js_scheduler.start}.
*)

(** {1 Javascript specific Eio functions.} *)

val sleep : float -> unit
(** [sleep d] waits for [d] seconds. *)

val yield : unit -> unit
(** [yield ()] suspends itself and then resumes as soon as
    possible. *)
