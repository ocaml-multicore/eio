open Std

type ty = [ `Vars ]
type 'a t = ([> ty] as 'a) r

val get_all : _ t -> (string * string) list
(** [all system] gets the full list of environment variables. *)

val get : _ t -> string -> string
(** [get_var system name] will get the environment variable called [name].
    If it does not exist, this function raises [Not_found]. *)

val put : _ t -> name:string -> value:string -> unit
(** [put_var system ~name ~value] adds the variable [name] associated with
    [value] to the environment. *)

module Pi : sig
  module type VARS = sig
    type t

    val get_all : t -> (string * string) list

    val get : t -> string -> string

    val put : t -> name:string -> value:string -> unit
  end

  val vars : (module VARS with type t = 't) -> ('t, ty) Resource.handler
end
