(** Trace Eio events using OCaml's runtime events system. *)

type id = private int
(** Each thread/fiber/promise is identified by a unique ID. *)

val mint_id : unit -> id
(** [mint_id ()] is a fresh unique [id]. *)

(** {2 Recording events}
    Libraries and applications can use these functions to make the traces more useful. *)

val log : string -> unit
(** [log msg] attaches text [msg] to the current fiber. *)

(** {2 Recording system events}
    These are normally only called by the scheduler. *)

val create : ?label:string -> id -> Eio_runtime_events.ty -> unit
(** [create id ty] records the creation of [id]. *)

val read : id -> unit
(** [read src] records that promise [src]'s value was read. *)

val try_read : id -> unit
(** [try_read src] records that the current fiber wants to read from [src] (which is not currently ready). *)

val fiber : id -> unit
(** [fiber id] records that [id] is now the current fiber for this domain. *)

val suspend : Runtime_events.Type.span -> unit
(** [suspend] records when the event loop is stopped waiting for events from the OS. *)

val resolve : id -> unit
(** [resolve id] records that [id] is now resolved. *)

val resolve_error : id -> exn -> unit
(** [resolve_error id exn] records that [id] is now failed. *)

val signal : id -> unit
(** [signal x] records that [x] was signalled. *)
