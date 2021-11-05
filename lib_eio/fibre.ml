open EffectHandlers

type _ eff += Fork : (unit -> 'a) -> 'a Promise.t eff
type _ eff += Yield : unit eff

let fork ~sw ~exn_turn_off f =
  let f () =
    Switch.with_op sw @@ fun () ->
    try f ()
    with ex ->
      if exn_turn_off then Switch.turn_off sw ex;
      raise ex
  in
  perform (Fork f)

type _ eff += Fork_ignore : (unit -> unit) -> unit eff

let fork_ignore ~sw f =
  let f () =
    Switch.with_op sw @@ fun () ->
    try f ()
    with ex ->
      Switch.turn_off sw ex;
      raise ex
  in
  perform (Fork_ignore f)

let yield () =
  perform Yield

let both ~sw f g =
  let x = fork ~sw ~exn_turn_off:true f in
  begin
    try g ()
    with ex -> Switch.turn_off sw ex
  end;
  ignore (Promise.await_result x : (unit, exn) result);
  match sw.state with
  | On _ -> ()
  | Off (ex, bt) ->
    Switch.raise_with_extras sw ex bt
  | Finished -> assert false

let fork_sub_ignore ?on_release ~sw ~on_error f =
  if Switch.is_finished sw then (
    (* If the switch is finished then we have no way to report the error after forking,
       so do it now. *)
    Option.iter (fun f -> f ()) on_release;
    invalid_arg "Switch finished!"
  );
  let f () =
    try Switch.sub ?on_release sw ~on_error f
    with ex ->
      Switch.turn_off sw ex;
      raise ex
  in
  perform (Fork_ignore f)
