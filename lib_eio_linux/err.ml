include Eio_unix.Err

let unclassified e = Eio.Exn.create (Eio.Exn.X e)

let run fn x =
  try fn x
  with Unix.Unix_error (code, name, arg) ->
    raise (v code name arg)
