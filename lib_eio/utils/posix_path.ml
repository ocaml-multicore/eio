(* Like [Filename.is_relative] but always using "/" as the separator. *)
let is_relative = function
  | "" -> true
  | x -> x.[0] <> '/'

(* Like [Filename.concat] but always using "/" as the separator. *)
let concat a b =
  let l = String.length a in
  if l = 0 || a.[l - 1] = '/' then a ^ b
  else a ^ "/" ^ b

let join p1 p2 =
  match p1, p2 with
  | p1, "" -> concat p1 p2
  | _, p2 when not (is_relative p2) -> p2
  | ".", p2 -> p2
  | p1, p2 -> concat p1 p2

(* Drop the first [n] characters from [s]. *)
let string_drop s n =
  String.sub s n (String.length s - n)

(* "/foo/bar//" -> "/foo/bar"
   "///" -> "/"
   "foo/bar" -> "foo/bar"
 *)
let remove_trailing_slashes s =
  let rec aux i =
    if i <= 1 || s.[i - 1] <> '/' then (
      if i = String.length s then s
      else String.sub s 0 i
    ) else aux (i - 1)
  in
  aux (String.length s)

let split p =
  match remove_trailing_slashes p with
  | "" -> None
  | "/" -> None
  | p ->
    match String.rindex_opt p '/' with
    | None -> Some ("", p)
    | Some idx ->
      let basename = string_drop p (idx + 1) in
      let dirname =
        if idx = 0 then "/"
        else remove_trailing_slashes (String.sub p 0 idx)
      in
      Some (dirname, basename)
