type 'a task = unit -> 'a

type t

val create : sw:Switch.t -> max_domains:int -> Domain_manager.t -> t

val run : t -> 'a task -> 'a

val async : t -> unit task -> unit

val async_promise : t -> 'a task -> 'a Promise.or_exn

val shutdown : t -> unit
