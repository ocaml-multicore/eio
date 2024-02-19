external eio_cap_enter : unit -> bool = "eio_unix_cap_enter"

let enter () =
  if eio_cap_enter () then Ok ()
  else Error `Not_supported
