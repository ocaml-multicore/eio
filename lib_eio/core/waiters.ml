type 'a waiter = {
  finished : bool Atomic.t;
  enqueue : ('a, exn) result -> unit;
}

type 'a t = 'a waiter Lwt_dllist.t

let create = Lwt_dllist.create

let add_waiter_protected ~mutex t cb =
  let w = Lwt_dllist.add_l cb t in
  Hook.Node_with_mutex (w, mutex)

let add_waiter t cb =
  let w = Lwt_dllist.add_l cb t in
  Hook.Node w

(* Wake a waiter with the result.
   Returns [false] if the waiter got cancelled while we were trying to wake it. *)
let wake { enqueue; finished } r =
  if Atomic.compare_and_set finished false true then (enqueue (Ok r); true)
  else false (* [cancel] gets called and we enqueue an error *)

let wake_all (t:_ t) v =
  try
    while true do
      let waiter = Lwt_dllist.take_r t in
      ignore (wake waiter v : bool)
    done
  with Lwt_dllist.Empty -> ()

let rec wake_one t v =
  match Lwt_dllist.take_opt_r t with
  | None -> `Queue_empty
  | Some waiter ->
    if wake waiter v then `Ok
    else wake_one t v

let is_empty = Lwt_dllist.is_empty

let await_internal ~mutex (t:'a t) id (ctx:Cancel.fiber_context) enqueue =
  match Cancel.Fiber_context.get_error ctx with
  | Some ex ->
    Option.iter Mutex.unlock mutex;
    enqueue (Error ex)
  | None ->
    let resolved_waiter = ref Hook.null in
    let finished = Atomic.make false in
    let enqueue x =
      Ctf.note_read ~reader:id ctx.tid;
      enqueue x
    in
    let cancel ex =
      if Atomic.compare_and_set finished false true then (
        Hook.remove !resolved_waiter;
        enqueue (Error ex)
      )
    in
    Cancel.Fiber_context.set_cancel_fn ctx cancel;
    let waiter = { enqueue; finished } in
    match mutex with
    | None ->
      resolved_waiter := add_waiter t waiter
    | Some mutex ->
      resolved_waiter := add_waiter_protected ~mutex t waiter;
      Mutex.unlock mutex

(* Returns a result if the wait succeeds, or raises if cancelled. *)
let await ~mutex waiters id =
  Suspend.enter_unchecked (await_internal ~mutex waiters id)
