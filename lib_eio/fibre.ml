open EffectHandlers

type _ eff += Fork : Cancel.fibre_context * (unit -> unit) -> unit eff

let yield () =
  let fibre = Suspend.enter (fun fibre enqueue -> enqueue (Ok fibre)) in
  Cancel.check fibre.cancel_context

let fork ~sw f =
  let f () =
    Switch.with_op sw @@ fun () ->
    try f ()
    with ex -> Switch.turn_off sw ex
  in
  let new_fibre = Cancel.Fibre_context.make ~cc:sw.cancel in
  perform (Fork (new_fibre, f))

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

let all xs =
  Switch.run @@ fun sw ->
  List.iter (fork ~sw) xs

let both f g = all [f; g]

let pair f g =
  Cancel.sub @@ fun cancel ->
  let x =
    let p, r = Promise.create () in
    let f () =
      match f () with
      | x -> Promise.fulfill r x
      | exception ex ->
        Cancel.cancel cancel ex;
        Promise.break r ex
    in
    let new_fibre = Cancel.Fibre_context.make ~cc:cancel in
    perform (Fork (new_fibre, f));
    p
  in
  match g () with
  | gr -> Promise.await x, gr               (* [g] succeeds - just report [f]'s result *)
  | exception gex ->
    Cancel.cancel cancel gex;
    match Cancel.protect (fun () -> Promise.await_result x) with
    | Ok _ | Error (Cancel.Cancelled _) -> raise gex    (* [g] fails, nothing to report for [f] *)
    | Error fex ->
      match gex with
      | Cancel.Cancelled _ -> raise fex                         (* [f] fails, nothing to report for [g] *)
      | _ -> raise (Multiple_exn.T [fex; gex])                  (* Both fail *)

let fork_sub ?on_release ~sw ~on_error f =
  let did_attach = ref false in
  fork ~sw (fun () ->
      try Switch.run (fun sw -> Option.iter (Switch.on_release sw) on_release; did_attach := true; f sw)
      with
      | Cancel.Cancelled _ as ex ->
        (* Don't report cancellation to [on_error] *)
        Switch.turn_off sw ex
      | ex ->
        try on_error ex
        with ex2 ->
          Switch.turn_off sw ex;
          Switch.turn_off sw ex2
    );
  if not !did_attach then (
    Option.iter Cancel.protect on_release;
    Switch.check sw;
    assert false
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
          | exception Cancel.Cancelled _ when Cancel.cancelled cc -> ()
          | exception ex ->
            begin match !r with
              | `None -> r := `Ex (ex, Printexc.get_raw_backtrace ()); Cancel.cancel cc ex
              | `Ok _ -> r := `Ex (ex, Printexc.get_raw_backtrace ())
              | `Ex (e1, bt) -> r := `Ex (Multiple_exn.T [e1; ex], bt)
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
  | `Ex (ex, bt), Some ex2 -> Printexc.raise_with_backtrace (Multiple_exn.T [ex; ex2]) bt
  | `None, None -> assert false

let first f g = any [f; g]
