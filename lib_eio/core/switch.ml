type t = {
  id : Ctf.id;
  mutable fibers : int;         (* Total, including daemon_fibers and the main function *)
  mutable daemon_fibers : int;
  mutable exs : (exn * Printexc.raw_backtrace) option;
  on_release : (unit -> unit) Lwt_dllist.t;
  waiter : unit Waiters.t;              (* The main [top]/[sub] function may wait here for fibers to finish. *)
  cancel : Cancel.t;
}

type hook =
  | Null
  | Hook : Domain.id * 'a Lwt_dllist.node -> hook

let null_hook = Null

let remove_hook = function
  | Null -> ()
  | Hook (id, n) ->
    if Domain.self () <> id then invalid_arg "Switch hook removed from wrong domain!";
    Lwt_dllist.remove n

let dump f t =
  Fmt.pf f "@[<v2>Switch %d (%d extra fibers):@,%a@]"
    (t.id :> int)
    t.fibers
    Cancel.dump t.cancel

let is_finished t = Cancel.is_finished t.cancel

(* Check switch belongs to this domain (and isn't finished). It's OK if it's cancelling. *)
let check_our_domain t =
  if is_finished t then invalid_arg "Switch finished!";
  if Domain.self () <> t.cancel.domain then invalid_arg "Switch accessed from wrong domain!"

(* Check isn't cancelled (or finished). *)
let check t =
  if is_finished t then invalid_arg "Switch finished!";
  Cancel.check t.cancel

let get_error t =
  Cancel.get_error t.cancel

let combine_exn ex = function
  | None -> ex
  | Some ex1 -> Exn.combine ex1 ex

(* Note: raises if [t] is finished or called from wrong domain. *)
let fail ?(bt=Printexc.get_raw_backtrace ()) t ex =
  check_our_domain t;
  if t.exs = None then
    Ctf.note_resolved t.id ~ex:(Some ex);
  t.exs <- Some (combine_exn (ex, bt) t.exs);
  try
    Cancel.cancel t.cancel ex
  with Exn.Cancel_hook_failed _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    t.exs <- Some (combine_exn (ex, bt) t.exs)

let inc_fibers t =
  check t;
  t.fibers <- t.fibers + 1

let dec_fibers t =
  t.fibers <- t.fibers - 1;
  if t.daemon_fibers > 0 && t.fibers = t.daemon_fibers then
    Cancel.cancel t.cancel Exit;
  if t.fibers = 0 then
    Waiters.wake_all t.waiter ()

let with_op t fn =
  inc_fibers t;
  Fun.protect fn
    ~finally:(fun () -> dec_fibers t)

let with_daemon t fn =
  inc_fibers t;
  t.daemon_fibers <- t.daemon_fibers + 1;
  Fun.protect fn
    ~finally:(fun () ->
        t.daemon_fibers <- t.daemon_fibers - 1;
        dec_fibers t
      )

let or_raise = function
  | Ok x -> x
  | Error ex -> raise ex

let rec await_idle t =
  (* Wait for fibers to finish: *)
  while t.fibers > 0 do
    Ctf.note_try_read t.id;
    Waiters.await ~mutex:None t.waiter t.id
  done;
  (* Call on_release handlers: *)
  let queue = Lwt_dllist.create () in
  Lwt_dllist.transfer_l t.on_release queue;
  let rec release () =
    match Lwt_dllist.take_opt_r queue with
    | None when t.fibers = 0 && Lwt_dllist.is_empty t.on_release -> ()
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
    fibers = 1;         (* The main function counts as a fiber *)
    daemon_fibers = 0;
    exs = None;
    waiter = Waiters.create ();
    on_release = Lwt_dllist.create ();
    cancel;
  }

let run_internal t fn =
  match fn t with
  | v ->
    dec_fibers t;
    await_idle t;
    Ctf.note_read t.id;
    maybe_raise_exs t;        (* Check for failure while finishing *)
    (* Success. *)
    v
  | exception ex ->
    (* Main function failed.
       Turn the switch off to cancel any running fibers, if it's not off already. *)
    dec_fibers t;
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
  Cancel.move_fiber_to t.cancel ctx;
  match fn () with
  | ()           -> Cancel.move_fiber_to old_cc ctx;
  | exception ex -> Cancel.move_fiber_to old_cc ctx; raise ex

let on_release_full t fn =
  if Domain.self () = t.cancel.domain then (
    match t.cancel.state with
    | On | Cancelling _ -> Lwt_dllist.add_r fn t.on_release
    | Finished ->
      match Cancel.protect fn with
      | () -> invalid_arg "Switch finished!"
      | exception ex -> raise (Exn.Multiple [ex; Invalid_argument "Switch finished!"])
  ) else (
    match Cancel.protect fn with
    | () -> invalid_arg "Switch accessed from wrong domain!"
    | exception ex -> raise (Exn.Multiple [ex; Invalid_argument "Switch accessed from wrong domain!"])
  )

let on_release t fn =
  ignore (on_release_full t fn : _ Lwt_dllist.node)

let on_release_cancellable t fn =
  Hook (t.cancel.domain, on_release_full t fn)
