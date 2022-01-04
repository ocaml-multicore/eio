type t = {
  id : Ctf.id;
  mutable fibres : int;
  mutable extra_exceptions : exn list;
  on_release : (unit -> unit) Lwt_dllist.t;
  waiter : unit Waiters.t;              (* The main [top]/[sub] function may wait here for fibres to finish. *)
  cancel : Cancel.t;
}

let is_finished t = Cancel.is_finished t.cancel

let check t =
  if is_finished t then invalid_arg "Switch finished!";
  Cancel.check t.cancel

let get_error t =
  Cancel.get_error t.cancel

let rec turn_off t ex =
  match t.cancel.state with
  | Finished -> invalid_arg "Switch finished!"
  | Cancelling (orig, _) when orig == ex || List.memq ex t.extra_exceptions -> ()
  | Cancelling _ ->
    begin match ex with
      | Cancel.Cancelled _ -> ()       (* The original exception will be reported elsewhere *)
      | Multiple_exn.T exns -> List.iter (turn_off t) exns
      | _ -> t.extra_exceptions <- ex :: t.extra_exceptions
    end
  | On ->
    Ctf.note_resolved t.id ~ex:(Some ex);
    Cancel.cancel t.cancel ex

let inc_fibres t =
  check t;
  t.fibres <- t.fibres + 1

let dec_fibres t =
  t.fibres <- t.fibres - 1;
  if t.fibres = 0 then
    Waiters.wake_all t.waiter ()

let with_op t fn =
  inc_fibres t;
  Fun.protect fn
    ~finally:(fun () -> dec_fibres t)

let or_raise = function
  | Ok x -> x
  | Error ex -> raise ex

let rec await_idle t =
  (* Wait for fibres to finish: *)
  while t.fibres > 0 do
    Ctf.note_try_read t.id;
    Waiters.await ~mutex:None t.waiter t.id
  done;
  (* Call on_release handlers: *)
  let queue = Lwt_dllist.create () in
  Lwt_dllist.transfer_l t.on_release queue;
  let rec release () =
    match Lwt_dllist.take_opt_r queue with
    | None when t.fibres = 0 && Lwt_dllist.is_empty t.on_release -> ()
    | None -> await_idle t
    | Some fn ->
      begin
        try fn () with
        | ex -> turn_off t ex
      end;
      release ()
  in
  release ()

let await_idle t = Cancel.protect (fun _ -> await_idle t)

let raise_with_extras t ex bt =
  match t.extra_exceptions with
  | [] -> Printexc.raise_with_backtrace ex bt
  | exns -> Printexc.raise_with_backtrace (Multiple_exn.T (ex :: List.rev exns)) bt

let create cancel =
  let id = Ctf.mint_id () in
  Ctf.note_created id Ctf.Switch;
  {
    id;
    fibres = 0;
    extra_exceptions = [];
    waiter = Waiters.create ();
    on_release = Lwt_dllist.create ();
    cancel;
  }

let run_internal t fn =
  match fn t with
  | v ->
    await_idle t;
    begin match t.cancel.state with
      | Finished -> assert false
      | On ->
        (* Success. *)
        Ctf.note_read t.id;
        v
      | Cancelling (ex, bt) ->
        (* Function succeeded, but got failure waiting for fibres to finish. *)
        Ctf.note_read t.id;
        raise_with_extras t ex bt
    end
  | exception ex ->
    (* Main function failed.
       Turn the switch off to cancel any running fibres, if it's not off already. *)
    begin
      try turn_off t ex
      with Cancel.Cancel_hook_failed _ as ex ->
        t.extra_exceptions <- ex :: t.extra_exceptions
    end;
    await_idle t;
    Ctf.note_read t.id;
    match t.cancel.state with
    | On | Finished -> assert false
    | Cancelling (ex, bt) -> raise_with_extras t ex bt

let run fn = Cancel.sub (fun cc -> run_internal (create cc) fn)

let run_protected fn =
  let ctx = EffectHandlers.perform Cancel.Get_context in
  Cancel.with_cc ~ctx ~parent:ctx.cancel_context ~protected:true @@ fun cancel ->
  run_internal (create cancel) fn

(* Run [fn ()] in [t]'s cancellation context.
   This prevents [t] from finishing until [fn] is done,
   and means that cancelling [t] will cancel [fn]. *)
let run_in t fn =
  with_op t @@ fun () ->
  let ctx = EffectHandlers.perform Cancel.Get_context in
  let old_cc = ctx.cancel_context in
  Cancel.move_fibre_to t.cancel ctx;
  match fn () with
  | ()           -> Cancel.move_fibre_to old_cc ctx;
  | exception ex -> Cancel.move_fibre_to old_cc ctx; raise ex

let on_release_full t fn =
  match t.cancel.state with
  | On | Cancelling _ -> Lwt_dllist.add_r fn t.on_release
  | Finished ->
    match Cancel.protect fn with
    | () -> invalid_arg "Switch finished!"
    | exception ex -> raise (Multiple_exn.T [ex; Invalid_argument "Switch finished!"])

let on_release t fn =
  ignore (on_release_full t fn : _ Lwt_dllist.node)

let on_release_cancellable t fn =
  Hook.Node (on_release_full t fn)
