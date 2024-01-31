(** This library is used to write event traces using OCaml's runtime events infrastructure. *)

type id = int

type obj_ty =
  | Promise
  | Semaphore
  | Stream
  | Mutex
(** Types of recorded objects. *)

val obj_ty_to_string : obj_ty -> string

type cc_ty =
  | Switch
  | Protect
  | Sub
  | Root
  | Any
(** Types of cancellation contexts. *)

val cc_ty_to_string : cc_ty -> string

(** {2 Writing events} *)

val create_fiber : (id * id) Runtime_events.User.t
val create_cc : (id * cc_ty) Runtime_events.User.t
val create_obj : (id * obj_ty) Runtime_events.User.t
val log : string Runtime_events.User.t
val enter_span : string Runtime_events.User.t
val exit_span : unit Runtime_events.User.t
val name : (id * string) Runtime_events.User.t
val suspend_fiber : string Runtime_events.User.t
val exit_cc : unit Runtime_events.User.t
val exit_fiber : id Runtime_events.User.t
val error : (id * exn) Runtime_events.User.t
val fiber : id Runtime_events.User.t
val get : id Runtime_events.User.t
val try_get : id Runtime_events.User.t
val put : id Runtime_events.User.t
val suspend_domain : Runtime_events.Type.span Runtime_events.User.t
val domain_spawn : id Runtime_events.User.t

(** {2 Consuming events} *)
    
type event = [
  | `Create of id * [
      | `Fiber_in of id         (** A new fiber is created in the given CC. *)
      | `Cc of cc_ty            (** The running fiber creates a new CC. *)
      | `Obj of obj_ty          (** The running fiber creates a new object. *)
    ]
  | `Fiber of id                (** The given fiber is now running. *)
  | `Name of id * string        (** Names a promise, stream, etc. *)
  | `Log of string              (** The running fiber logs a message. *)
  | `Enter_span of string       (** The running fiber enters a traced section. *)
  | `Exit_span                  (** The running fiber leaves the current traced section. *)
  | `Get of id                  (** The running fiber gets a value from a promise, stream, acquires a lock, etc. *)
  | `Try_get of id              (** The running fiber wants to get, but must wait. *)
  | `Put of id                  (** The running fiber resolves a promise, adds to a stream, releases a lock etc. *)
  | `Error of (id * string)     (** A CC fails with the given error. *)
  | `Exit_cc                    (** The current CC ends. *)
  | `Exit_fiber of id           (** The running fiber ends. *)
  | `Suspend_domain of Runtime_events.Type.span  (** The domain asks the OS to wait for events. *)
  | `Suspend_fiber of string    (** The running fiber is suspended (until resumed by [`Fiber]). *)
  | `Domain_spawn of id         (** The current domain was spawned by fiber [id]. *)
]

val pp_event : Format.formatter -> event -> unit
(** [pp_event] formats an event as a human-readable string *)

val add_callbacks:
  (int -> Runtime_events.Timestamp.t -> event -> unit) ->
  Runtime_events.Callbacks.t -> Runtime_events.Callbacks.t
(** [add_callbacks fn x] adds event handler [fn] to [x].

    When an Eio event is processed, it calls [fn ring_id ts event]. *)
