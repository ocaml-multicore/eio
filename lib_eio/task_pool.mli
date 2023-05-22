type 'a task = unit -> 'a

type t

val create : sw:Switch.t -> max_domains:int -> Domain_manager.t -> t

val run : t -> 'a task -> 'a

val async : t -> 'a task -> 'a Promise.t
