type 'a t

val create :
  ?init_size:int ->
  ?check:('a -> bool) ->
  ?dispose:('a -> unit) ->
  alloc:(unit -> 'a) ->
  int ->
  'a t

val use : 'a t -> ('a -> 'b) -> 'b

val async : 'a t -> ('a -> 'b) -> 'b Promise.t

val clear : 'a t -> unit
