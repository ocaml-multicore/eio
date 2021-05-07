type 'a t
(** A queue of callbacks waiting for a value of type ['a]. *)

val create : unit -> 'a t

val add_waiter : 'a t -> (('a, exn) result -> unit) -> unit

val wake_all : 'a t -> ('a, exn) result -> unit
