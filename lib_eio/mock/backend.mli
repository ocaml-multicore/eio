(** A dummy Eio backend with no actual IO. *)

val run : (unit -> 'a) -> 'a
(** [run fn] runs an event loop and then calls [fn env] within it. *)
