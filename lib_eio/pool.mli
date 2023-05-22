type 'a t

type 'a handlers = {
  check : 'a -> bool;
  dispose : 'a -> unit;
}

val noop_handlers : 'a handlers

val create :
  ?init_size:int ->
  alloc:(unit -> 'a * 'a handlers) ->
  int ->
  'a t

val use : 'a t -> ('a -> 'b) -> 'b

val async : sw:Switch.t -> 'a t -> ('a -> 'b) -> 'b Promise.t

val clear : 'a t -> unit
