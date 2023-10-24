(** This library is used to write event traces in mirage-profile's CTF format. *)

type id = private int
(** Each thread/fiber/promise is identified by a unique ID. *)

(** {2 Recording events}
    Libraries and applications can use these functions to make the traces more useful. *)

val label : string -> unit
(** [label msg] attaches text [msg] to the current thread. *)

(** {2 Recording system events}
    These are normally only called by the scheduler. *)

type ty =
  | Fiber
  | Promise
  | Semaphore
  | Switch
  | Stream
  | Mutex
(** Types of recorded objects. *)

val mint_id : unit -> id
(** [mint_id ()] is a fresh unique [id]. *)

val create : ?label:string -> id -> ty -> unit
(** [create t id ty] records the creation of [id]. *)

val read : ?reader:id -> id -> unit
(** [read src] records that promise [src]'s value was read.
    @param reader The thread doing the read (default is the current thread). *)

val try_read : id -> unit
(** [try_read src] records that the current thread wants to read from [src] (which is not currently ready). *)

val fiber : id -> unit
(** [fiber id] records that fiber [id] is now running. *)

val hiatus : unit -> unit
(** [hiatus ()] records that the system will sleep for reason [r]. *)

val resume : id -> unit
(** [resume id] records that the system has resumed (used after {!hiatus}),
    and is now running [id]. *)

val resolve : id -> ex:exn option -> unit
(** [resolve id ~ex] records that [id] is now resolved.
    If [ex = None] then [id] was successful, otherwise it failed with exception [ex]. *)

val signal : ?src:id -> id -> unit
(** [signal ~src dst] records that [dst] was signalled.
    @param src The thread sending the signal (default is the current thread). *)

(** {2 Controlling tracing} *)

type log_buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Control : sig
  type t

  val make : timestamper:(log_buffer -> int -> unit) -> log_buffer -> t
  (** [make ~timestamper b] is a trace buffer that record events in [b].
      In most cases, the {!Eio_unix.Trace} module provides a simpler interface. *)

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
