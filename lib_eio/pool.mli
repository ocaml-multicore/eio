type 'a t

type 'a handlers = {
  check : 'a -> bool;
  dispose : 'a -> unit;
}

val noop_handlers : 'a handlers

val create :
  sw:Switch.t ->
  alloc:(unit -> 'a * 'a handlers) ->
  int ->
  'a t

val use : 'a t -> ('a -> 'b) -> 'b

val async : sw:Switch.t -> 'a t -> ('a -> unit) -> unit

val async_promise : sw:Switch.t -> 'a t -> ('a -> 'b) -> 'b Promise.or_exn

val clear : 'a t -> unit

val shutdown : 'a t -> unit
