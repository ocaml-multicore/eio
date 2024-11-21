type t = {
  mutable fibers : int;         (* Total, including daemon_fibers and the main function *)
  mutable daemon_fibers : int;
  mutable exs : (exn * Printexc.raw_backtrace) option;
  on_release_lock : Mutex.t;
  mutable on_release : (unit -> unit) Lwt_dllist.t option;      (* [None] when closed. *)
  waiter : unit Single_waiter.t;              (* The main [top]/[sub] function may wait here for fibers to finish. *)
  cancel : Cancel.t;
}

type hook =
  | Null
  | Hook : Mutex.t * (unit -> unit) Lwt_dllist.node -> hook

let null_hook = Null

let cancelled () = assert false

let try_remove_hook = function
  | Null -> false
  | Hook (on_release_lock, n) ->
    Mutex.lock on_release_lock;
    Lwt_dllist.remove n;
    let fn = Lwt_dllist.get n in
    Lwt_dllist.set n cancelled;
    Mutex.unlock on_release_lock;
    fn != cancelled

let remove_hook x = ignore (try_remove_hook x : bool)

let dump f t =
  Fmt.pf f "@[<v2>Switch %d (%d extra fibers):@,%a@]"
    (t.cancel.id :> int)
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
let fail ?(bt=Exn.empty_backtrace) t ex =
  check_our_domain t;
  t.exs <- Some (combine_exn (ex, bt) t.exs);
  try
    Cancel.cancel t.cancel ex
  with ex ->
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
    Single_waiter.wake_if_sleeping t.waiter

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
    Trace.try_get t.cancel.id;
    Single_waiter.await_protect t.waiter "Switch.await_idle" t.cancel.id
  done;
  (* Collect on_release handlers: *)
  let queue = ref [] in
  let enqueue n =
    let fn = Lwt_dllist.get n in
    Lwt_dllist.set n cancelled;
    queue := fn :: !queue
  in
  Mutex.lock t.on_release_lock;
  Option.iter (Lwt_dllist.iter_node_l enqueue) t.on_release;
  t.on_release <- None;
  Mutex.unlock t.on_release_lock;
  (* Run on_release handlers *)
  !queue |> List.iter (fun fn -> try Cancel.protect fn with ex -> fail t ex);
  if t.fibers > 0 then await_idle t

let maybe_raise_exs t =
  match t.exs with
  | None -> ()
  | Some (ex, bt) -> Printexc.raise_with_backtrace ex bt

let create cancel =
  {
    fibers = 1;         (* The main function counts as a fiber *)
    daemon_fibers = 0;
    exs = None;
    waiter = Single_waiter.create ();
    on_release_lock = Mutex.create ();
    on_release = Some (Lwt_dllist.create ());
    cancel;
  }

let run_internal t fn =
  match fn t with
  | v ->
    dec_fibers t;
    await_idle t;
    Trace.get t.cancel.id;
    maybe_raise_exs t;        (* Check for failure while finishing *)
    (* Success. *)
    v
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    (* Main function failed.
       Turn the switch off to cancel any running fibers, if it's not off already. *)
    dec_fibers t;
    fail ~bt t ex;
    await_idle t;
    Trace.get t.cancel.id;
    maybe_raise_exs t;
    assert false

let run ?name fn = Cancel.sub_checked ?name Switch (fun cc -> run_internal (create cc) fn)

let run_protected ?name fn =
  let ctx = Effect.perform Cancel.Get_context in
  Cancel.with_cc ~ctx ~parent:ctx.cancel_context ~protected:true Switch @@ fun cancel ->
  Option.iter (Trace.name cancel.id) name;
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

exception Release_error of string * exn

let () =
  Printexc.register_printer (function
      | Release_error (msg, ex) -> Some (Fmt.str "@[<v2>%s@,while handling %a@]" msg Exn.pp ex)
      | _ -> None
    )

let on_release_full t fn =
  Mutex.lock t.on_release_lock;
  match t.on_release with
  | Some handlers ->
    let node = Lwt_dllist.add_r fn handlers in
    Mutex.unlock t.on_release_lock;
    node
  | None ->
    Mutex.unlock t.on_release_lock;
    match Cancel.protect fn with
    | () -> invalid_arg "Switch finished!"
    | exception ex ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace (Release_error ("Switch finished!", ex)) bt

let on_release t fn =
  ignore (on_release_full t fn : _ Lwt_dllist.node)

let on_release_cancellable t fn =
  Hook (t.on_release_lock, on_release_full t fn)
