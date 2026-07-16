include Eio_unix.Err

type Eio.Exn.Backend.t +=
  | Outside_sandbox of string
  | Absolute_path
  | Invalid_leaf of string

let () =
  Eio.Exn.Backend.register_pp (fun f -> function
      | Outside_sandbox path -> Fmt.pf f "Outside_sandbox (%S)" path; true
      | Absolute_path -> Fmt.pf f "Absolute_path"; true
      | Invalid_leaf x -> Fmt.pf f "Invalid_leaf %S" x; true
      | _ -> false
    )

let run fn x =
  try fn x
  with Unix.Unix_error (code, name, arg) ->
    raise (v code name arg)
