(** This library is used to write event traces in mirage-profile's CTF format. *)

type id = private int
(** Each thread/fiber/promise is identified by a unique ID. *) 

(** {2 Recording events}
    Libraries and applications can use these functions to make the traces more useful. *)

val label : string -> unit
(** [label msg] attaches text [msg] to the current thread. *)

val note_increase : string -> int -> unit
(** [note_increase counter delta] records that [counter] increased by [delta].
    If [delta] is negative, this records a decrease. *)

val note_counter_value : string -> int -> unit
(** [note_counter_value counter value] records that [counter] is now [value]. *)

val should_resolve : id -> unit
(** [should_resolve id] records that [id] is expected to resolve, and should be highlighted if it doesn't. *)

(** {2 Recording system events}
    These are normally only called by the scheduler. *)

type hiatus_reason =
  | Wait_for_work
  | Suspend
  | Hibernate

type event =
  | Wait
  | Task
  | Bind
  | Try
  | Choose
  | Pick
  | Join
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
(** Types of threads or other recorded objects. *)

val mint_id : unit -> id
(** [mint_id ()] is a fresh unique [id]. *)

val note_created : ?label:string -> id -> event -> unit
(** [note_created t id ty] records the creation of [id]. *)

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

val note_fork : unit -> id
(** [note_fork ()] records that a new thread has been forked and returns a fresh ID for it. *)

val note_resolved : id -> ex:exn option -> unit
(** [note_resolved id ~ex] records that [id] is now resolved.
    If [ex = None] then [id] was successful, otherwise it failed with exception [ex]. *)

val note_signal : ?src:id -> id -> unit
(** [note_signal ~src dst] records that [dst] was signalled.
    @param src The thread sending the signal (default is the current thread). *)

(** {2 Controlling tracing} *)

type log_buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Control : sig
  type t

  val make : timestamper:(log_buffer -> int -> unit) -> log_buffer -> t
  (** [make ~timestamper b] is a trace buffer that record events in [b].
      In most cases, the {!Ctf_unix} module provides a simpler interface. *)

  val start : t -> unit
  (** [start t] begins recording events in [t]. *)

  val stop : t -> unit
  (** [stop t] stops recording to [t] (which must be the current trace buffer). *)
end

(**/**)

module BS : sig
  val set_int8 : Cstruct.buffer -> int -> int -> unit
  val set_int64_le : Cstruct.buffer -> int -> int64 -> unit
end
