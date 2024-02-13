type token =
  | Empty
  | DotDot
  | String of string

let rec tokenise = function
  | [] -> []
  | ["."] -> [Empty]                  (* "path/." is the same as "path/" *)
  | "." :: xs -> tokenise xs          (* Skip dot if not at end *)
  | "" :: xs -> Empty :: tokenise xs
  | ".." :: xs -> DotDot :: tokenise xs
  | x :: xs -> String x :: tokenise xs

module Rel = struct
  type t =
    | Leaf of { basename : string; trailing_slash : bool }
    | Self    (* A final "." *)
    | Child of string * t
    | Parent of t

  let rec parse = function
    | [] -> Self
    | [String basename; Empty] -> Leaf { basename; trailing_slash = true }
    | [String basename] -> Leaf { basename; trailing_slash = false }
    | [DotDot] -> Parent Self
    | DotDot :: xs -> Parent (parse xs)
    | String s :: xs -> Child (s, parse xs)
    | Empty :: xs -> parse xs

  let parse s = parse (tokenise s)

  let rec concat a b =
    match a with
    | Leaf { basename; trailing_slash = _ } -> Child (basename, b)
    | Child (name, xs) -> Child (name, concat xs b)
    | Parent xs -> Parent (concat xs b)
    | Self -> b

  let rec dump f = function
    | Child (x, xs) -> Fmt.pf f "%S / %a" x dump xs
    | Parent xs -> Fmt.pf f ".. / %a" dump xs
    | Self -> Fmt.pf f "."
    | Leaf { basename; trailing_slash } ->
      Fmt.pf f "%S" basename;
      if trailing_slash then Fmt.pf f " /"

  let rec segs = function
    | Leaf { basename; trailing_slash } -> [if trailing_slash then basename ^ "/" else basename]
    | Self -> [""]
    | Child (x, xs) -> x :: segs xs
    | Parent Self -> [".."]
    | Parent xs -> ".." :: segs xs

  let to_string = function
    | Self -> "."
    | t -> String.concat "/" (segs t)
end

type t =
  | Relative of Rel.t
  | Absolute of Rel.t

let rec parse_abs = function
  | "" :: [] -> Absolute Self
  | "" :: xs -> parse_abs xs
  | xs -> Absolute (Rel.parse xs)

let parse = function
  | "" -> Relative Self
  | s ->
    match String.split_on_char '/' s with
    | "" :: path -> parse_abs path
    | path -> Relative (Rel.parse path)

let dump f = function
  | Relative r -> Rel.dump f r
  | Absolute r -> Fmt.pf f "/ %a" Rel.dump r

let to_string = function
  | Relative r -> Rel.to_string r
  | Absolute r -> String.concat "/" ("" :: Rel.segs r)
