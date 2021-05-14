effect Fork  : Switch.t * bool * (unit -> 'a) -> 'a Promise.t
let fork ~sw ~exn_turn_off f =
  perform (Fork (sw, exn_turn_off, f))

effect Fork_ignore : Switch.t * (unit -> unit) -> unit
let fork_ignore ~sw f =
  perform (Fork_ignore (sw, f))

effect Yield : unit
let yield ?sw () =
  perform Yield;
  Option.iter Switch.check sw

let both ~sw f g =
  let x = fork ~sw ~exn_turn_off:true f in
  begin
    try g ()
    with ex -> Switch.turn_off sw ex
  end;
  Promise.await x;
  Switch.check sw

let fork_sub_ignore ~sw ~on_error f =
  fork_ignore ~sw (fun () -> Switch.sub ~sw ~on_error f)
