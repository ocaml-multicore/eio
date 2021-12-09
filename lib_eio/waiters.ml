type 'a t = ('a -> unit) Lwt_dllist.t

let create = Lwt_dllist.create

let add_waiter_protected ~mutex t cb =
  let w = Lwt_dllist.add_l cb t in
  Hook.Node_with_mutex (w, mutex)

let add_waiter t cb =
  let w = Lwt_dllist.add_l cb t in
  Hook.Node w

let wake_all t v =
  try
    while true do
      Lwt_dllist.take_r t v
    done
  with Lwt_dllist.Empty -> ()

let wake_one t v =
  match Lwt_dllist.take_opt_r t with
  | None -> `Queue_empty
  | Some f -> f v; `Ok

let is_empty = Lwt_dllist.is_empty

let await_internal ~mutex (t:'a t) id (ctx:Cancel.fibre_context) enqueue =
  match Cancel.Fibre_context.get_error ctx with
  | Some ex ->
    Option.iter Mutex.unlock mutex;
    enqueue (Error ex)
  | None ->
    let resolved_waiter = ref Hook.null in
    let enqueue x =
      Ctf.note_read ~reader:id ctx.tid;
      enqueue x
    in
    let cancel ex =
      Hook.remove !resolved_waiter;
      enqueue (Error ex)
    in
    Cancel.Fibre_context.set_cancel_fn ctx cancel;
    let when_resolved r =
      if Cancel.Fibre_context.clear_cancel_fn ctx then enqueue (Ok r)
      (* else [cancel] gets called and we enqueue an error *)
    in
    match mutex with
    | None ->
      resolved_waiter := add_waiter t when_resolved
    | Some mutex ->
      resolved_waiter := add_waiter_protected ~mutex t when_resolved;
      Mutex.unlock mutex

(* Returns a result if the wait succeeds, or raises if cancelled. *)
let await ~mutex waiters id =
  Suspend.enter_unchecked (await_internal ~mutex waiters id)
