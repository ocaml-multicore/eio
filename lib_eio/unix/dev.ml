type t = Eio.File.Dev.t

external make_raw : int -> int -> int64 = "eio_unix_makedev"
external major_raw : int64 -> int = "eio_unix_dev_major"
external minor_raw : int64 -> int = "eio_unix_dev_minor"

let major t = major_raw (Eio.File.Dev.to_int64 t)
let minor t = minor_raw (Eio.File.Dev.to_int64 t)

let pp f t = Fmt.pf f "%d:%d" (major t) (minor t)

let make ~major:maj ~minor:mnr =
  if maj < 0 || mnr < 0 then
    Fmt.invalid_arg "Dev.make: negative major or minor number (%d:%d)" maj mnr;
  let t = Eio.File.Dev.of_int64 (make_raw maj mnr) in
  (* [makedev] silently truncates, so check by taking it apart again. *)
  if major t <> maj || minor t <> mnr then
    Fmt.invalid_arg
      "Dev.make: %d:%d is out of range for this platform (it became %a)"
      maj mnr pp t;
  t
