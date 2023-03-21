let unclassified e = Eio.Exn.create (Eio.Exn.X e)

let wrap code name arg =
  let ex = Eio_unix.Unix_error (code, name, arg) in
  match code with
  | ECONNREFUSED -> Eio.Net.err (Connection_failure (Refused ex))
  | ECONNRESET | EPIPE -> Eio.Net.err (Connection_reset ex)
  | _ -> unclassified ex

let wrap_fs code name arg =
  let e = Eio_unix.Unix_error (code, name, arg) in
  match code with
  | Unix.EEXIST -> Eio.Fs.err (Already_exists e)
  | Unix.ENOENT -> Eio.Fs.err (Not_found e)
  | Unix.EXDEV | EPERM | EACCES -> Eio.Fs.err (Permission_denied e)
  | _ -> wrap code name arg
