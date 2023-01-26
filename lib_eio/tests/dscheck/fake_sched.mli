val run : (unit -> unit) -> Eio.Cancel.t option
(** [run fn] runs [fn ()] in a new fiber and returns its context so it can be cancelled.

    [fn] may suspend at most once.
    If it doesn't suspend then [run] returns [None] after it finishes. *)

val cancel : Eio.Cancel.t -> unit
(** [cancel ctx] cancels the context with a suitable dummy exception. *)
