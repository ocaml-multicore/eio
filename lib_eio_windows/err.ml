include Eio_unix.Err

type Eio.Exn.Backend.t +=
  | Outside_sandbox of string * string
  | Absolute_path
  | Invalid_leaf of string

let beneath ~root path = function
  | Some p -> p
  | None -> raise (Eio.Fs.err (Permission_denied (Outside_sandbox (path, root))))

let relative ~root path =
  let module Path = Eio_utils.Nt_path in
  if not (Path.is_relative path) then raise (Eio.Fs.err (Permission_denied Absolute_path));
  beneath ~root path (Path.relative path)

let unclassified_error e = Eio.Exn.create (Eio.Exn.X e)

let () =
  Eio.Exn.Backend.register_pp (fun f -> function
      | Outside_sandbox (path, dir) -> Fmt.pf f "Outside_sandbox (%S, %S)" path dir; true
      | Absolute_path -> Fmt.pf f "Absolute_path"; true
      | Invalid_leaf x -> Fmt.pf f "Invalid_leaf %S" x; true
      | _ -> false
    )

let run fn x =
  try fn x
  with Unix.Unix_error (code, name, arg) ->
    raise (v code name arg)
