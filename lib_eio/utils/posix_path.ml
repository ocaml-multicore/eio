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

let split p =
  let rec skip_slashes i = if i > 0 && p.[i - 1] = '/' then skip_slashes (i - 1) else i in
  let rec skip_step i = if i > 0 && p.[i - 1] <> '/' then skip_step (i - 1) else i in
  let base_end = skip_slashes (String.length p) in
  if base_end = 0 then None
  else (
    let base_start = skip_step base_end in
    let base =
      if base_start = 0 && base_end = String.length p then p
      else String.sub p base_start (base_end - base_start)
    in
    if base_start = 0 then Some ("", base)
    else match skip_slashes base_start with
      | 0 -> Some ("/", base)
      | dir_end -> Some (String.sub p 0 dir_end, base)
  )
