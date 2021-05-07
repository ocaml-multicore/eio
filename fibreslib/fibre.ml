effect Fork  : (unit -> 'a) -> 'a Promise.t
let fork f =
  perform (Fork f)

effect Fork_detach  : (unit -> unit) * (exn -> unit) -> unit
let fork_detach f ~on_error =
  perform (Fork_detach (f, on_error))

effect Yield : unit
let yield () =
  perform Yield
