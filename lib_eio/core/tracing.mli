type id = private int

val mint_id : unit -> id
(** [mint_id ()] is a fresh unique [id]. *)

type hiatus_reason = Wait_for_work

type cancellation_context =
  | Choose
  | Pick
  | Join
  | Switch
  | Protect
  | Sub
  | Root

type event =
  | Wait
  | Task
  | Bind
  | Try
  | Map
  | Condition
  | On_success
  | On_failure
  | On_termination
  | On_any
  | Ignore_result
  | Async
  | Promise
  | Semaphore
  | Switch
  | Stream
  | Mutex
  | Cancellation_context of {
    purpose: cancellation_context;
    protected: bool;
  }
  | System_thread
(** Types of threads or other recorded objects. *)

(** {2 Recording events}
    Libraries and applications can use these functions to make the traces more useful. *)

val log : string -> unit
(** [log msg] attaches text [msg] to the current thread. *)

val set_loc : string -> unit
(** [set_loc msg] attaches location [msg] to the current thread. *)

val set_name : string -> unit
(** [set_name msg] attaches name [msg] to the current thread. *)

(** {2 Recording system events}
    These are normally only called by the scheduler. *)

val note_created : ?label:string -> ?loc:string -> id -> event -> unit
(** [note_created t id ty] records the creation of [id]. *)

val note_loc : id -> string -> unit
(** [note_loc msg] attaches location [msg] to [id]. *)

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

type log_buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Control : sig
  (** Using runtime events (only available after OCaml 5.1) *)

  val start : unit -> unit
  (** [start t] begins recording events in [t]. *)

  val stop : unit -> unit
  (** [stop t] stops recording to [t] (which must be the current trace buffer). *)

  (** Common trace format *)

  type ctf

  val make : timestamper:(log_buffer -> int -> unit) -> log_buffer -> ctf
  (** [make ~timestamper b] is a trace buffer that record events in [b].
      In most cases, the {!Ctf_unix} module provides a simpler interface. *)

  val start_ctf : ctf -> unit
  (** [start_ctf t] begins recording events in [t]. *)

  val stop_ctf : ctf -> unit
  (** [stop_ctf t] stops recording to [t] (which must be the current trace buffer). *)
end

(**/**)

val get_caller : unit -> string

module BS : sig
  val set_int8 : Cstruct.buffer -> int -> int -> unit
  val set_int64_le : Cstruct.buffer -> int -> int64 -> unit
end
