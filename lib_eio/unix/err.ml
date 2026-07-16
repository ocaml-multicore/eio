type Eio.Exn.Backend.t += Unix_error of Unix.error * string * string

let () =
  Eio.Exn.Backend.register_pp (fun f -> function
      | Unix_error (code, name, arg) -> Fmt.pf f "Unix_error (%s, %S, %S)" (Unix.error_message code) name arg; true
      | _ -> false
    )

let unclassified e = Eio.Exn.create (Eio.Exn.X e)

let v code name arg =
  let e = Unix_error (code, name, arg) in
  match code with
  | EUNKNOWNERR x when Some x = Config.enotcapable -> Eio.Fs.err (Permission_denied e)
  | ECONNREFUSED -> Eio.Net.err (Connection_failure (Refused e))
  | ECONNRESET | EPIPE | ECONNABORTED -> Eio.Net.err (Connection_reset e)
  | ENOPROTOOPT -> Eio.Net.err Invalid_option
  | ENOSYS | EOPNOTSUPP -> Eio.Exn.create (Eio.Exn.Not_available e)
  | EEXIST -> Eio.Fs.err (Already_exists e)
  | ENOENT -> Eio.Fs.err (Not_found e)
  | EXDEV | EACCES | EPERM -> Eio.Fs.err (Permission_denied e)
  | _ -> unclassified e
