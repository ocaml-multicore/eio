type Eio.Exn.Backend.t +=
  | Outside_sandbox of string
  | Absolute_path
  | Invalid_leaf of string

let unclassified_error e = Eio.Exn.create (Eio.Exn.X e)

let () =
  Eio.Exn.Backend.register_pp (fun f -> function
      | Outside_sandbox path -> Fmt.pf f "Outside_sandbox (%S)" path; true
      | Absolute_path -> Fmt.pf f "Absolute_path"; true
      | Invalid_leaf x -> Fmt.pf f "Invalid_leaf %S" x; true
      | _ -> false
    )

let wrap code name arg =
  let e = Eio_unix.Unix_error (code, name, arg) in
  match code with
  | EEXIST -> Eio.Fs.err (Already_exists e)
  | ENOENT -> Eio.Fs.err (Not_found e)
  | EXDEV | EACCES | EPERM -> Eio.Fs.err (Permission_denied e)
  | EUNKNOWNERR x when Some x = Config.enotcapable -> Eio.Fs.err (Permission_denied e)
  | ECONNREFUSED -> Eio.Net.err (Connection_failure (Refused e))
  | ECONNRESET | EPIPE -> Eio.Net.err (Connection_reset e)
  | _ -> unclassified_error e

let run fn x =
  try fn x
  with Unix.Unix_error (code, name, arg) ->
    raise (wrap code name arg)
