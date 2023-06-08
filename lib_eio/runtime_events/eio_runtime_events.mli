(** This library is used to write event traces using OCaml's runtime events infrastructure. *)

type id = private int
(** Each thread/fiber/promise is identified by a unique ID. *)

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

val event_to_string : event -> string

(** Eio event types and tags *)

val created_type : (id * event) Runtime_events.Type.t

type Runtime_events.User.tag += Created

val labelled_type : (id * string) Runtime_events.Type.t

type Runtime_events.User.tag += Failed | Log | Name

val two_ids_type : (id * id) Runtime_events.Type.t

type Runtime_events.User.tag += Read | Try_read | Signal | Parent

(* int type *)

type Runtime_events.User.tag += Resolved | Switch

(* unit type *)

type Runtime_events.User.tag += Suspend

(** Producing events *)

val note_created : id -> event  -> unit

val note_read : reader:id -> id -> unit

val note_try_read : id -> id -> unit

val note_signal : src:id -> id -> unit

val note_resolved : id -> ex:exn option -> unit

val note_log : id -> string -> unit

val note_name : id -> string -> unit

val note_switch : id -> unit

val note_hiatus : hiatus_reason -> unit

val note_parent : child:id -> parent:id -> unit
