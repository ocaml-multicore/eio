type 'a t

val create : unit -> 'a t
val add_waiter : 'a t -> (('a, exn) result -> unit) -> unit
val wake_all : 'a t -> ('a, exn) result -> unit
val wake_one : 'a t -> ('a, exn) result -> [`Ok | `Queue_empty]
val await : 'a t -> Ctf.id -> 'a

effect Await : Ctf.id * 'a t -> 'a
