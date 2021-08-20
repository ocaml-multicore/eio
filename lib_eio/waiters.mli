type 'a t

type waiter = unit -> unit

val create : unit -> 'a t
val add_waiter : 'a t -> (('a, exn) result -> unit) -> waiter
val wake_all : 'a t -> ('a, exn) result -> unit
val wake_one : 'a t -> ('a, exn) result -> [`Ok | `Queue_empty]
val remove_waiter : waiter -> unit
val is_empty : 'a t -> bool
