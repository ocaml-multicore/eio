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
