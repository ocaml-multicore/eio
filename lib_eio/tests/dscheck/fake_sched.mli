val run : (unit -> unit) -> Eio.Cancel.t option
(** [run fn] runs [fn ()] in a new fiber and returns its context so it can be cancelled.

    Returns None if it never suspended. *)

val cancel : Eio.Cancel.t -> unit
(** [cancel ctx] cancels the context with a suitable dummy exception. *)
