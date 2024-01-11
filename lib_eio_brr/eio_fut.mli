val await : ?abort:(unit -> unit) -> 'a Fut.t -> 'a
(** [await ?abort f] waits for the completion of future [f] and
    returns its value. The optional [abort] function is called when
    the fiber is cancelled. *)

val await_exn : ?abort:(unit -> unit) -> 'a Fut.or_error -> 'a
(** [await ?abort f] waits for the completion of future [f] and
    returns its value. An exception is raised in case of a JavaScript
    error. The optional [abort] function is called when the fiber is
    cancelled. *)

val make : sw:Eio.Switch.t -> (unit -> 'a) -> 'a Fut.t
(** [make ~sw fn] runs [fn] in a new fiber (attached to [sw]) and
    returns a future whose value is the return value of the
    function. *)

val make_exn : sw:Eio.Switch.t -> (unit -> 'a) -> 'a Fut.or_error
(** [make ~sw fn] runs [fn] in a new fiber (attached to [sw]) and
    returns a future whose value is either the return value of the
    function or a JavaScript error if the functions fails with a
    JavaScript exception. *)
