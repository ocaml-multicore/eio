(* Windows path syntax. *)

let is_drive_letter = function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false
let is_sep c = c = '\\' || c = '/'
let backslashes = String.map (fun c -> if c = '/' then '\\' else c)

(* A recognizer matches at position [i] of [s] and returns the position
   one past the match, or [None] if it doesn't match. *)
type recognizer = string -> int -> int option

let charp f : recognizer =
  fun s i -> if i < String.length s && f s.[i] then Some (i + 1) else None

let ( *> ) (p : recognizer) (q : recognizer) : recognizer =
  fun s i -> Option.bind (p s i) (q s)

let ( <|> ) (p : recognizer) (q : recognizer) : recognizer =
  fun s i -> match p s i with Some _ as r -> r | None -> q s i

let opt p : recognizer = p <|> (fun _ i -> Some i)

(* Zero or more matches of [p]. *)
let rec many p : recognizer = fun s i ->
  match p s i with Some j -> many p s j | None -> Some i

let matches (r : recognizer) s = Option.is_some (r s 0)

let chr c = charp (Char.equal c)
let eos : recognizer = fun s i -> if i = String.length s then Some i else None

let word w : recognizer = fun s i ->
  let n = String.length w in
  if i + n <= String.length s && String.lowercase_ascii (String.sub s i n) = w then Some (i + n) else None

let any_word ws = List.fold_left (fun r w -> r <|> word w) (fun _ _ -> None) ws

let sep = charp is_sep
let bslash = chr '\\'
let qmark = chr '?'

(* The (possibly empty) run of non-separator characters at [i]. *)
let component = many (charp (fun c -> not (is_sep c)))

(* A component whose text satisfies [f]. *)
let component_is f : recognizer = fun s i ->
  match component s i with
  | Some e when f (String.sub s i (e - i)) -> Some e
  | _ -> None

(* The component after this one. [opt sep] rather than [sep] so that a
   malformed prefix is cut at whatever exists. *)
let next = opt sep *> component

let drive = charp is_drive_letter *> chr ':'
let q_or_dot = component_is (fun c -> c = "?" || c = ".")
let unc_kw = component_is (fun c -> String.uppercase_ascii c = "UNC")

(* The "C:", "\\server\share", "\\.\device", "\\?\..." or "\??\..."
   volume prefix of a path. *)
let volume_prefix =
  drive                                                  (* C: *)
  <|> (sep *> sep *> q_or_dot *> opt sep *> unc_kw
       *> next *> next)                                  (* \\?\UNC\server\share *)
  <|> (sep *> sep *> component *> next)                  (* \\server\share, \\?\C: or \\.\device *)
  <|> (bslash *> qmark *> qmark *> component *> next)    (* \??\C: - the NT object-manager form (backslash only) *)

(* Win32 does no normalization in the verbatim and NT namespaces. *)
let verbatim_prefix = bslash *> (bslash <|> qmark) *> qmark
let verbatim s = Option.is_some (verbatim_prefix s 0)

(* [\??\], [\\?\] and [\\.\] all name the NT object-manager namespace. *)
let nt_prefix = (verbatim_prefix <|> (bslash *> bslash *> chr '.')) *> bslash

type kind = [ `Relative | `Rooted | `Drive_relative | `Absolute | `Unc | `Device | `Verbatim ]

let pp_kind ppf : kind -> unit = function
  | `Relative -> Fmt.string ppf "relative"
  | `Rooted -> Fmt.string ppf "rooted"
  | `Drive_relative -> Fmt.string ppf "drive-relative"
  | `Absolute -> Fmt.string ppf "absolute"
  | `Unc -> Fmt.string ppf "UNC"
  | `Device -> Fmt.string ppf "device"
  | `Verbatim -> Fmt.string ppf "verbatim"

(* See https://learn.microsoft.com/en-us/dotnet/standard/io/file-path-formats#identify-the-path *)
let classify p : kind =
  if verbatim p then `Verbatim
  else if matches (sep *> sep *> q_or_dot) p then `Device
  else if matches (sep *> sep) p then `Unc
  else if matches (drive *> sep) p then `Absolute
  else if matches drive p then `Drive_relative
  else if matches sep p then `Rooted
  else `Relative

let is_relative p = classify p = `Relative

let volume_end s = Option.value (volume_prefix s 0) ~default:0
let drop n s = String.sub s n (String.length s - n)

(* A path's volume prefix, and the rest of the path. *)
let split_volume p =
  let n = volume_end p in
  String.sub p 0 n, drop n p

let split p =
  let vend = volume_end p in
  let sep_char = if verbatim p then Char.equal '\\' else is_sep in
  let sep_at i = sep_char p.[i] in
  (* Trailing separators are ignored; one is kept for a bare root. *)
  let rec trim i = if i > vend + 1 && sep_at (i - 1) then trim (i - 1) else i in
  let stop = trim (String.length p) in
  if stop <= vend || (stop = vend + 1 && sep_at vend) then None
  else
    let rec rsep i = if i < vend then None else if sep_at i then Some i else rsep (i - 1) in
    match rsep (stop - 1) with
    | None -> Some (String.sub p 0 vend, String.sub p vend (stop - vend))
    | Some idx ->
      let basename = String.sub p (idx + 1) (stop - idx - 1) in
      let dirname =
        (* keep the root separator itself *)
        String.sub p 0 (if idx = vend then vend + 1 else trim idx)
      in
      Some (dirname, basename)

let parent_and_leaf p =
  match split p with
  | Some ("", leaf) -> ".", leaf
  | Some parts -> parts
  | None -> ".", (if p = "" then "." else p)

let dirname p = fst (parent_and_leaf p)
let basename p = snd (parent_and_leaf p)

let concat a b =
  let l = String.length a in
  if l = 0 then b
  else if (if verbatim a then a.[l - 1] = '\\' else is_sep a.[l - 1]) then a ^ b
  else if drive a 0 = Some l then
    a ^ b   (* a bare drive is drive-relative: adding a separator would change its meaning *)
  else a ^ "\\" ^ b

let join p1 p2 =
  match p1, p2 with
  | p1, "" -> concat p1 p2
  | _, p2 when not (is_relative p2) -> p2
  | ".", p2 -> p2
  | p1, p2 -> concat p1 p2

let chop s = String.sub s 0 (String.length s - 1)

let rec trim_end s =
  if String.ends_with ~suffix:"." s || String.ends_with ~suffix:" " s then trim_end (chop s) else s

(* https://learn.microsoft.com/en-us/dotnet/standard/io/file-path-formats#path-normalization *)
let normalise_components path =
  let rec go ~above acc = function
    | [] -> let acc = List.rev acc in if above then `Escaped acc else `Within acc
    | ("" | ".") :: xs -> go ~above acc xs
    | ".." :: xs -> (match acc with _ :: acc -> go ~above acc xs | [] -> go ~above:true [] xs)
    | [x] -> go ~above (match trim_end x with "" -> acc | x -> x :: acc) []
    | x :: xs when String.ends_with ~suffix:"." x && not (String.ends_with ~suffix:".." x) -> go ~above (chop x :: acc) xs
    | x :: xs -> go ~above (x :: acc) xs
  in
  go ~above:false [] (String.split_on_char '\\' path)

let normalise path =
  match normalise_components path with
  | `Within cs | `Escaped cs -> "\\" ^ String.concat "\\" cs

let after r p = Option.map (fun i -> drop i p) (r p 0)

(* [qualify p] is the absolute Win32 path [p] named in the NT namespace. *)
let qualify p =
  "\\??\\" ^
  match after nt_prefix p, after (bslash *> bslash) p with
  | Some rest, _ -> rest                     (* \??\, \\?\ or \\.\ *)
  | None, Some share -> "UNC\\" ^ share      (* \\server\share *)
  | None, None -> p                          (* C:\... *)

let to_nt ~cwd p =
  let vol, rest = split_volume (backslashes p) in
  let cwd_vol, cwd_rest = split_volume (backslashes cwd) in
  let resolve vol base = qualify (vol ^ normalise (base ^ "\\" ^ rest)) in
  match classify p with
  | `Verbatim -> qualify p
  | `Relative -> resolve cwd_vol cwd_rest
  | `Rooted -> resolve cwd_vol ""
  | `Drive_relative when String.uppercase_ascii vol = String.uppercase_ascii cwd_vol -> resolve cwd_vol cwd_rest
  | `Drive_relative       (* We can't see other drives' current directories, so use the root *)
  | `Absolute | `Unc | `Device -> resolve vol ""

(* Names reserved for devices, from
   https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file#naming-conventions
   The docs say these are reserved even with an extension. Win32 also ignores
   trailing spaces and a ":" (an empty stream name), so "NUL ." and "NUL:" are
   devices too, as are "NUL.txt" on older versions of Windows. *)
let reserved_name =
  (any_word ["con"; "prn"; "aux"; "nul"]
   <|> (any_word ["com"; "lpt"] *> any_word ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "¹"; "²"; "³"]))
  *> many (chr ' ') *> (chr '.' <|> chr ':' <|> eos)

let to_win32 p =
  match after (verbatim_prefix *> bslash) p with
  | None -> p
  | Some rest ->
    let win32 =
      match after (unc_kw *> bslash) rest with
      | Some share -> "\\\\" ^ share
      | None -> rest
    in
    if to_nt ~cwd:"" win32 = "\\??\\" ^ rest
    && not (List.exists (matches reserved_name) (String.split_on_char '\\' win32))
    then win32
    else "\\\\?\\" ^ rest

let beneath ~root p =
  match classify p with
  | `Relative ->
    (match normalise_components (backslashes p) with
     | `Within components -> Some components
     | `Escaped _ -> None)
  | _ ->
    let components p = String.split_on_char '\\' (to_nt ~cwd:root p) |> List.filter (( <> ) "") in
    let rec strip = function
      | [], rest -> Some rest
      | r :: root, x :: rest when String.lowercase_ascii r = String.lowercase_ascii x -> strip (root, rest)
      | _ -> None
    in
    strip (components root, components p)
