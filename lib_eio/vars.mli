open Std

type ty = [ `Vars ]
type 'a t = ([> ty] as 'a) r

(** {1 Accessing environment variables} *)

val get_all : _ t -> (string * string) list
(** [get_all vars] gets the full list of environment variables. *)

val get : _ t -> string -> string
(** [get vars name] will get the environment variable called [name].

    @raise Not_found if [name] does not exist.*)

val get_path : _ t -> string list
(** [get_path var] gets the ["PATH"] variable and returns the paths as a list.

    @raise Not_found if ["PATH"] does not exist.*)

(** {1 Setting environment variables} *)

val put : _ t -> name:string -> value:string -> unit
(** [put vars ~name ~value] adds the environment variables [name=value] to the
    environment. *)

val put_path : _ t -> string list -> unit
(** [put_path vars paths] will set the ["PATH"] environment variable [paths]. *)

module Pi : sig
  module type VARS = sig
    type t

    val get_all : t -> (string * string) list

    val get : t -> string -> string

    val get_path : t -> string list

    val put : t -> name:string -> value:string -> unit

    val put_path : t -> string list -> unit
  end

  val vars : (module VARS with type t = 't) -> ('t, ty) Resource.handler
end
