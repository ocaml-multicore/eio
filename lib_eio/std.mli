module Promise = Eio__core.Promise
module Fiber = Eio__core.Fiber
module Switch = Eio__core.Switch

type 'a r = 'a Resource.t

val traceln :
  ?__POS__:string * int * int * int ->
  ('a, Format.formatter, unit, unit) format4 -> 'a
  (** Same as {!Eio.traceln}. *)
