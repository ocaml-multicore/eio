type file_descr = [`Open | `Closed] Atomic.t

let make () = Atomic.make `Open

let close t =
  if not (Atomic.compare_and_set t `Open `Closed) then
    failwith "Already closed!"
