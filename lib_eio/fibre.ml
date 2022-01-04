open EffectHandlers

type _ eff += Fork : Cancel.fibre_context * (unit -> unit) -> unit eff

let yield () =
  let fibre = Suspend.enter (fun fibre enqueue -> enqueue (Ok fibre)) in
  Cancel.check fibre.cancel_context

let fork_raw cc f =
  let new_fibre = Cancel.Fibre_context.make ~cc in
  perform (Fork (new_fibre, f))

let fork ~sw f =
  fork_raw sw.Switch.cancel @@ fun () ->
  Switch.with_op sw @@ fun () ->
  try f ()
  with ex -> Switch.turn_off sw ex

let fork_promise ~sw f =
  let new_fibre = Cancel.Fibre_context.make ~cc:sw.Switch.cancel in
  let p, r = Promise.create_with_id (Cancel.Fibre_context.tid new_fibre) in
  let f () =
    match Switch.with_op sw f with
    | x -> Promise.fulfill r x
    | exception ex -> Promise.break r ex
  in
  perform (Fork (new_fibre, f));
  p

let fork_on_accept ~on_handler_error ~sw:adopting_sw accept handle =
  (* Create a new sub-switch of [adopting_sw].
     Run [accept] with the new switch as an argument,
     but itself still running in the original context.
     This situation is unusual because we have a switch but we're not in [Switch.run],
     so we have to make sure we finish it safely in all cases. *)
  Switch.check adopting_sw;
  let cc = Cancel.create ~protected:false in
  let deactivate = Cancel.activate cc ~parent:adopting_sw.cancel in
  let child_switch = Switch.create cc in
  (* We must prevent [adopting_sw] from finishing while it has a child switch. *)
  Switch.inc_fibres adopting_sw;
  let run_child fn =
    match Switch.run_internal child_switch (fun (_ : Switch.t) -> fn ()) with
    | () -> deactivate (); Switch.dec_fibres adopting_sw
    | exception ex -> deactivate (); Switch.dec_fibres adopting_sw; raise ex
  in
  (* From this point we have a [child_switch] that may have fibres and other resources attached to it.
     We must call [run_child] on it in all cases. *)
  match accept child_switch with
  | exception ex ->
    (* Accept failed. Don't fork. Shut down [child_switch] in the parent context. *)
    run_child (fun () -> raise ex)
  | _ when not (Cancel.is_on adopting_sw.cancel) ->
    (* We can't fork into [adopting_sw] if it's cancelled, so just clean up and report Cancelled.
       The main error is owned by [adopting_sw]. *)
    begin
      try run_child ignore
      with ex -> raise (Cancel.Cancelled ex)
    end;
    assert false        (* [run_child] must have failed if its parent is cancelled *)
  | x ->
    (* Accept succeeded. Fork a new fibre into [adopting_sw] and
       run it with [child_switch] as its context. *)
    fork_raw child_switch.cancel @@ fun () ->
    try run_child (fun () -> Switch.check child_switch; handle child_switch x)
    with ex ->
      (* No point reporting an error if we're being cancelled. Also, nowhere to run it. *)
      if Cancel.is_on adopting_sw.cancel then (
        Switch.run_in adopting_sw @@ fun () ->
        try on_handler_error ex
        with ex2 ->
          Switch.turn_off adopting_sw ex;
          Switch.turn_off adopting_sw ex2
      )

let all xs =
  Switch.run @@ fun sw ->
  List.iter (fork ~sw) xs

let both f g = all [f; g]

let pair f g =
  Cancel.sub @@ fun cc ->
  let x =
    let p, r = Promise.create () in
    let f () =
      match f () with
      | x -> Promise.fulfill r x
      | exception ex ->
        Cancel.cancel cc ex;
        Promise.break r ex
    in
    let new_fibre = Cancel.Fibre_context.make ~cc in
    perform (Fork (new_fibre, f));
    p
  in
  match g () with
  | gr -> Promise.await x, gr               (* [g] succeeds - just report [f]'s result *)
  | exception gex ->
    Cancel.cancel cc gex;
    (* Note: we don't know if the [Cancelled] exceptions here are from us cancelling or from an external
       cancel, but it doesn't matter. If the cancel was external then the parent context is cancelled,
       and [Cancel.sub] checks for us at the end. *)
    match Cancel.protect (fun () -> Promise.await_result x) with
    | Ok _ | Error (Cancel.Cancelled _) -> raise gex    (* [g] fails, nothing to report for [f] *)
    | Error fex ->
      match gex with
      | Cancel.Cancelled _ -> raise fex                         (* [f] fails, nothing to report for [g] *)
      | _ -> raise (Cancel.combine_exn fex gex)                 (* Both fail *)

let fork_sub ~sw ~on_error f =
  fork ~sw (fun () ->
      try Switch.run f
      with
      | ex when Cancel.is_on sw.cancel ->
        (* Typically the caller's context is within [sw], but it doesn't have to be.
           It's possible that the original context has finished by now,
           but [fork] is keeping [sw] alive so we can use that report the error. *)
        Switch.run_in sw @@ fun () ->
        try on_error ex
        with ex2 ->
          Switch.turn_off sw ex;
          Switch.turn_off sw ex2
    )

exception Not_first

let await_cancel () =
  Suspend.enter @@ fun fibre enqueue ->
  Cancel.Fibre_context.set_cancel_fn fibre (fun ex -> enqueue (Error ex))

let any fs =
  let r = ref `None in
  let parent_c =
    Cancel.sub_unchecked (fun cc ->
        let wrap h =
          match h () with
          | x ->
            begin match !r with
              | `None -> r := `Ok x; Cancel.cancel cc Not_first
              | `Ex _ | `Ok _ -> ()
            end
          | exception Cancel.Cancelled _ when not (Cancel.is_on cc) ->
            (* If this is in response to us asking the fibre to cancel then we can just ignore it.
               If it's in response to our parent context being cancelled (which also cancels [cc]) then
               we'll check that context and raise it at the end anyway. *)
            ()
          | exception ex ->
            begin match !r with
              | `None -> r := `Ex (ex, Printexc.get_raw_backtrace ()); Cancel.cancel cc ex
              | `Ok _ -> r := `Ex (ex, Printexc.get_raw_backtrace ())
              | `Ex (e1, bt) -> r := `Ex (Cancel.combine_exn e1 ex, bt)
            end
        in
        let rec aux = function
          | [] -> await_cancel ()
          | [f] -> wrap f; []
          | f :: fs ->
            let new_fibre = Cancel.Fibre_context.make ~cc in
            let p, r = Promise.create () in
            let f () =
              match wrap f with
              | x -> Promise.fulfill r x
              | exception ex -> Promise.break r ex
            in
            perform (Fork (new_fibre, f));
            p :: aux fs
        in
        let ps = aux fs in
        Cancel.protect (fun () -> List.iter Promise.await ps)
      )
  in
  match !r, Cancel.get_error parent_c with
  | `Ok r, None -> r
  | (`Ok _ | `None), Some ex -> raise ex
  | `Ex (ex, bt), None -> Printexc.raise_with_backtrace ex bt
  | `Ex (ex, bt), Some ex2 -> Printexc.raise_with_backtrace (Cancel.combine_exn ex ex2) bt
  | `None, None -> assert false

let first f g = any [f; g]

let check () =
  let ctx = perform Cancel.Get_context in
  Cancel.check ctx.cancel_context
