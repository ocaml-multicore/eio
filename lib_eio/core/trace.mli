(** Trace Eio events using OCaml's runtime events system. *)

type id = private int
(** Each thread/fiber/promise is identified by a unique ID. *)

val mint_id : unit -> id
(** [mint_id ()] is a fresh unique [id]. *)

(** {2 Recording events}
    Libraries and applications can use these functions to make the traces more useful. *)

val log : string -> unit
(** [log msg] attaches text [msg] to the current fiber. *)

val name : id -> string -> unit
(** [name id label] sets [label] as the name for [id]. *)

val with_span : string -> (unit -> 'a) -> 'a
(** [with_span op fn] runs [fn ()], labelling the timespan during which it runs with [op]. *)

val suspend_fiber : string -> unit
(** [suspend_fiber op] records that the current fiber is now suspended waiting for [op]. *)

(** {2 Recording system events}
    These are normally only called by the scheduler. *)

val create_fiber : cc:id -> id -> unit
(** [create_fiber ~cc id] records the creation of fiber [id] in context [cc]. *)

val create_cc : id -> Eio_runtime_events.cc_ty -> unit
(** [create_cc id ty] records the creation of cancellation context [id]. *)

val create_obj : ?label:string -> id -> Eio_runtime_events.obj_ty -> unit
(** [create_obj id ty] records the creation of [id]. *)

val get : id -> unit
(** [get src] records reading a promise, taking from a stream, taking a lock, etc. *)

val try_get : id -> unit
(** [try_get src] records that the current fiber wants to get from [src] (which is not currently ready). *)

val put : id -> unit
(** [put dst] records resolving a promise, adding to a stream, releasing a lock, etc. *)

val fiber : id -> unit
(** [fiber id] records that [id] is now the current fiber for this domain. *)

val suspend_domain : Runtime_events.Type.span -> unit
(** [suspend_domain] records when the event loop is stopped waiting for events from the OS. *)

val domain_spawn : parent:id -> unit
(** [domain_spawn ~parent] records that the current domain was spawned by fiber [parent]. *)

val exit_cc : unit -> unit
(** [exit_cc ()] records that the current CC has finished. *)

val exit_fiber : id -> unit
(** [exit_fiber id] records that fiber [id] has finished. *)

val error : id -> exn -> unit
(** [error id exn] records that [id] received an error. *)
