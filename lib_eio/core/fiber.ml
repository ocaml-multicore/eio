type _ Effect.t += Fork : Cancel.fiber_context * (unit -> unit) -> unit Effect.t

let yield () =
  let fiber = Suspend.enter (fun fiber enqueue -> enqueue (Ok fiber)) in
  Cancel.check fiber.cancel_context

(* Note: [f] must not raise an exception, as that would terminate the whole scheduler. *)
let fork_raw new_fiber f =
  Effect.perform (Fork (new_fiber, f))

let fork ~sw f =
  Switch.check_our_domain sw;
  if Cancel.is_on sw.cancel then (
    let new_fiber = Cancel.Fiber_context.make ~cc:sw.cancel in
    fork_raw new_fiber @@ fun () ->
    Switch.with_op sw @@ fun () ->
    match f () with
    | () ->
      Ctf.note_resolved (Cancel.Fiber_context.tid new_fiber) ~ex:None
    | exception ex ->
      Switch.fail sw ex;  (* The [with_op] ensures this will succeed *)
      Ctf.note_resolved (Cancel.Fiber_context.tid new_fiber) ~ex:(Some ex)
  ) (* else the fiber should report the error to [sw], but [sw] is failed anyway *)

let fork_promise ~sw f =
  Switch.check_our_domain sw;
  let new_fiber = Cancel.Fiber_context.make ~cc:sw.Switch.cancel in
  let p, r = Promise.create_with_id (Cancel.Fiber_context.tid new_fiber) in
  fork_raw new_fiber (fun () ->
      match Switch.with_op sw f with
      | x -> Promise.resolve_ok r x
      | exception ex -> Promise.resolve_error r ex        (* Can't fail; only we have [r] *)
    );
  p

let fork_on_accept ~on_handler_error ~sw:adopting_sw accept handle =
  (* Create a new sub-switch of [adopting_sw].
     Run [accept] with the new switch as an argument,
     but itself still running in the original context.
     This situation is unusual because we have a switch but we're not in [Switch.run],
     so we have to make sure we finish it safely in all cases. *)
  Switch.check_our_domain adopting_sw;
  Switch.check adopting_sw;
  let cc = Cancel.create ~protected:false in
  let deactivate = Cancel.activate cc ~parent:adopting_sw.cancel in
  let child_switch = Switch.create cc in
  (* We must prevent [adopting_sw] from finishing while it has a child switch. *)
  Switch.inc_fibers adopting_sw;
  let run_child fn =
    match Switch.run_internal child_switch (fun (_ : Switch.t) -> fn ()) with
    | () -> deactivate (); Switch.dec_fibers adopting_sw; Switch.check adopting_sw
    | exception ex -> deactivate (); Switch.dec_fibers adopting_sw; raise ex
  in
  (* From this point we have a [child_switch] that may have fibers and other resources attached to it.
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
      with
      | Cancel.Cancelled _ as ex -> raise ex
      | ex -> raise (Cancel.Cancelled ex)
    end;
    assert false        (* [run_child] must have failed if its parent is cancelled *)
  | x ->
    (* Accept succeeded. Fork a new fiber into [adopting_sw] and
       run it with [child_switch] as its context. *)
    let new_fiber = Cancel.Fiber_context.make ~cc:child_switch.cancel in
    fork_raw new_fiber @@ fun () ->
    match run_child (fun () -> Switch.check child_switch; handle child_switch x) with
    | () ->
      Ctf.note_resolved (Cancel.Fiber_context.tid new_fiber) ~ex:None
    | exception ex ->
      (* No point reporting an error if we're being cancelled. Also, nowhere to run it. *)
      if Cancel.is_on adopting_sw.cancel then (
        Switch.run_in adopting_sw @@ fun () ->
        try on_handler_error ex
        with ex2 ->
          (* The [run_in] ensures [adopting_sw] isn't finished here *)
          Switch.fail adopting_sw ex;
          Switch.fail adopting_sw ex2
      );
      Ctf.note_resolved (Cancel.Fiber_context.tid new_fiber) ~ex:(Some ex)

let all xs =
  Switch.run @@ fun sw ->
  List.iter (fork ~sw) xs

let both f g = all [f; g]

let pair f g =
  Switch.run @@ fun sw ->
  let x = fork_promise ~sw f in
  let y = g () in
  (Promise.await_exn x, y)

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
          (* The [run_in] ensures [adopting_sw] isn't finished here *)
          Switch.fail sw ex;
          Switch.fail sw ex2
    )

exception Not_first

let await_cancel () =
  Suspend.enter @@ fun fiber enqueue ->
  Cancel.Fiber_context.set_cancel_fn fiber (fun ex -> enqueue (Error ex))

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
            (* If this is in response to us asking the fiber to cancel then we can just ignore it.
               If it's in response to our parent context being cancelled (which also cancels [cc]) then
               we'll check that context and raise it at the end anyway. *)
            ()
          | exception ex ->
            begin match !r with
              | `None -> r := `Ex (ex, Printexc.get_raw_backtrace ()); Cancel.cancel cc ex
              | `Ok _ -> r := `Ex (ex, Printexc.get_raw_backtrace ())
              | `Ex prev ->
                let bt = Printexc.get_raw_backtrace () in
                r := `Ex (Exn.combine prev (ex, bt))
            end
        in
        let rec aux = function
          | [] -> await_cancel ()
          | [f] -> wrap f; []
          | f :: fs ->
            let new_fiber = Cancel.Fiber_context.make ~cc in
            let p, r = Promise.create_with_id (Cancel.Fiber_context.tid new_fiber) in
            fork_raw new_fiber (fun () ->
                match wrap f with
                | x -> Promise.resolve_ok r x
                | exception ex -> Promise.resolve_error r ex
              );
            p :: aux fs
        in
        let ps = aux fs in
        Cancel.protect (fun () -> List.iter Promise.await_exn ps)
      )
  in
  match !r, Cancel.get_error parent_c with
  | `Ok r, None -> r
  | (`Ok _ | `None), Some ex -> raise ex
  | `Ex (ex, bt), None -> Printexc.raise_with_backtrace ex bt
  | `Ex ex1, Some ex2 ->
    let bt2 = Printexc.get_raw_backtrace () in
    let ex, bt = Exn.combine ex1 (ex2, bt2) in
    Printexc.raise_with_backtrace ex bt
  | `None, None -> assert false

let first f g = any [f; g]

let check () =
  let ctx = Effect.perform Cancel.Get_context in
  Cancel.check ctx.cancel_context
