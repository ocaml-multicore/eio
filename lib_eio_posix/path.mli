module Rel : sig
  type t =
    | Leaf of { basename : string; trailing_slash : bool }
    | Self    (* A final "." *)
    | Child of string * t
    | Parent of t

  val concat : t -> t -> t

  val to_string : t -> string

  val dump : t Fmt.t
end

type t =
  | Relative of Rel.t
  | Absolute of Rel.t

val parse : string -> t
(** Note:
    [parse "" = Relative Self]
    [parse ".." = Relative (Parent Self)] *)

val to_string : t -> string

val dump : t Fmt.t
