include module type of Eio_runtime_events


(** {2 Recording events}
    Libraries and applications can use these functions to make the traces more useful. *)

val log : string -> unit
(** [log msg] attaches text [msg] to the current thread. *)

val set_name : string -> unit
(** [set_name msg] attaches name [msg] to the current thread. *)

(** {2 Recording system events}
    These are normally only called by the scheduler. *)

val note_created : ?label:string -> id -> event -> unit
(** [note_created t id ty] records the creation of [id]. *)

val note_name : id -> string -> unit
(** [note_name msg] attaches name [msg] to [id]. *)

val note_parent : child:id -> parent:id -> unit
(** [note_parent ~child ~parent] attaches [child] fiber to the given [parent] context. *)

val note_read : ?reader:id -> id -> unit
(** [note_read src] records that promise [src]'s value was read.
    @param reader The thread doing the read (default is the current thread). *)

val note_try_read : id -> unit
(** [note_try_read src] records that the current thread wants to read from [src] (which is not currently ready). *)

val note_switch : id -> unit
(** [note_switch id] records that [id] is now the current thread. *)

val note_hiatus : hiatus_reason -> unit
(** [note_hiatus r] records that the system will sleep for reason [r]. *)

val note_resume : id -> unit
(** [note_resume id] records that the system has resumed (used after {!note_hiatus}),
    and is now running [id]. *)

val note_resolved : id -> ex:exn option -> unit
(** [note_resolved id ~ex] records that [id] is now resolved.
    If [ex = None] then [id] was successful, otherwise it failed with exception [ex]. *)

val note_signal : ?src:id -> id -> unit
(** [note_signal ~src dst] records that [dst] was signalled.
    @param src The thread sending the signal (default is the current thread). *)

(** {2 Controlling tracing} *)

module Control : sig
  val start : unit -> unit
  (** [start t] begins recording events in [t]. *)

  val stop : unit -> unit
  (** [stop t] stops recording to [t] (which must be the current trace buffer). *)
end
