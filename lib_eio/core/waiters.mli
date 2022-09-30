(* See [eio__core.mli] for details. *)

type 'a t

val create : unit -> 'a t

val wake_all : 'a t -> 'a -> unit

val wake_one : 'a t -> 'a -> [`Ok | `Queue_empty]

val is_empty : 'a t -> bool

val await :
  mutex:Mutex.t option ->
  'a t -> Ctf.id -> 'a

val await_internal :
  mutex:Mutex.t option ->
  'a t -> Ctf.id -> Cancel.fiber_context ->
  (('a, exn) result -> unit) -> unit
