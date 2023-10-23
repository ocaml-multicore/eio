(** This library is used to write event traces using OCaml's runtime events infrastructure. *)

type id = int

type ty =
  | Fiber
  | Promise
  | Semaphore
  | Switch
  | Stream
  | Mutex
(** Types of recorded objects. *)

val ty_to_string : ty -> string

(** {2 Writing events} *)

val create : (id * ty) Runtime_events.User.t
val log : string Runtime_events.User.t
val name : (id * string) Runtime_events.User.t
val resolve : id Runtime_events.User.t
val resolve_error : (id * exn) Runtime_events.User.t
val fiber : id Runtime_events.User.t
val read : id Runtime_events.User.t
val try_read : id Runtime_events.User.t
val signal : id Runtime_events.User.t
val suspend : Runtime_events.Type.span Runtime_events.User.t

(** {2 Consuming events} *)

type 'a handler = int -> Runtime_events.Timestamp.t -> 'a -> unit
(** A ['a handler] is a function for handling events of type ['a].
    It is called as [handler ring_id ts value]. *)

val add_callbacks:
  ?create:(id * ty) handler ->
  ?read:id handler ->
  ?try_read:id handler ->
  ?resolve:id handler ->
  ?resolve_error:(id * string) handler ->
  ?name:(id * string) handler ->
  ?log:string handler ->
  ?fiber:id handler ->
  ?signal:id handler ->
  ?suspend:Runtime_events.Type.span handler ->
  Runtime_events.Callbacks.t -> Runtime_events.Callbacks.t
