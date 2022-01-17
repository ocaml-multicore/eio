type t = {
  id : Ctf.id;
  mutable fibres : int;
  mutable exs : (exn * Printexc.raw_backtrace) option;
  on_release : (unit -> unit) Lwt_dllist.t;
  waiter : unit Waiters.t;              (* The main [top]/[sub] function may wait here for fibres to finish. *)
  cancel : Cancel.t;
}

let dump f t =
  Fmt.pf f "@[<v2>Switch %d (%d extra fibres):@,%a@]"
    (t.id :> int)
    t.fibres
    Cancel.dump t.cancel

let is_finished t = Cancel.is_finished t.cancel

let check t =
  if is_finished t then invalid_arg "Switch finished!";
  Cancel.check t.cancel

let get_error t =
  Cancel.get_error t.cancel

let combine_exn ex = function
  | None -> ex
  | Some ex1 -> Exn.combine ex1 ex

let fail ?(bt=Printexc.get_raw_backtrace ()) t ex =
  if t.exs = None then
    Ctf.note_resolved t.id ~ex:(Some ex);
  t.exs <- Some (combine_exn (ex, bt) t.exs);
  try
    Cancel.cancel t.cancel ex
  with Exn.Cancel_hook_failed _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    t.exs <- Some (combine_exn (ex, bt) t.exs)

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
        | ex -> fail t ex
      end;
      release ()
  in
  release ()

let await_idle t = Cancel.protect (fun _ -> await_idle t)

let maybe_raise_exs t =
  match t.exs with
  | None -> ()
  | Some (ex, bt) -> Printexc.raise_with_backtrace ex bt

let create cancel =
  let id = Ctf.mint_id () in
  Ctf.note_created id Ctf.Switch;
  {
    id;
    fibres = 0;
    exs = None;
    waiter = Waiters.create ();
    on_release = Lwt_dllist.create ();
    cancel;
  }

let run_internal t fn =
  match fn t with
  | v ->
    await_idle t;
    Ctf.note_read t.id;
    maybe_raise_exs t;        (* Check for failure while finishing *)
    (* Success. *)
    v
  | exception ex ->
    (* Main function failed.
       Turn the switch off to cancel any running fibres, if it's not off already. *)
    fail t ex;
    await_idle t;
    Ctf.note_read t.id;
    maybe_raise_exs t;
    assert false

let run fn = Cancel.sub (fun cc -> run_internal (create cc) fn)

let run_protected fn =
  let ctx = Effect.perform Cancel.Get_context in
  Cancel.with_cc ~ctx ~parent:ctx.cancel_context ~protected:true @@ fun cancel ->
  run_internal (create cancel) fn

(* Run [fn ()] in [t]'s cancellation context.
   This prevents [t] from finishing until [fn] is done,
   and means that cancelling [t] will cancel [fn]. *)
let run_in t fn =
  with_op t @@ fun () ->
  let ctx = Effect.perform Cancel.Get_context in
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
    | exception ex -> raise (Exn.Multiple [ex; Invalid_argument "Switch finished!"])

let on_release t fn =
  ignore (on_release_full t fn : _ Lwt_dllist.node)

let on_release_cancellable t fn =
  Hook.Node (on_release_full t fn)
