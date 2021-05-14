let switch_finished = Invalid_argument "Switch finished!"

type state =
  | On of (exn -> unit) Lwt_dllist.t
  | Off of exn

type t = {
  id : Ctf.id;
  mutable state : state;
  mutable fibres : int;
  waiter : unit Waiters.t;
}

effect Await : t option * Ctf.id * 'a Waiters.t -> 'a

let await ?sw waiters id =
  perform (Await (sw, id, waiters))

let check t =
  match t.state with
  | On _ -> ()
  | Off ex -> raise ex

let turn_off t ex =
  match t.state with
  | Off _ -> ()
  | On q ->
    if ex == switch_finished then
      Ctf.note_resolved t.id ~ex:None
    else
      Ctf.note_resolved t.id ~ex:(Some ex);
    t.state <- Off ex;
    Lwt_dllist.iter_r (fun f -> f ex) q

let add_cancel_hook t hook =
  match t.state with
  | Off ex -> hook ex; ignore
  | On q ->
    let node = Lwt_dllist.add_r hook q in
    (fun () -> Lwt_dllist.remove node)

let add_cancel_hook_opt t hook =
  match t with
  | Some t -> add_cancel_hook t hook
  | None -> ignore

let with_op t fn =
  match t.state with
  | Off ex -> raise ex
  | On _ ->
    t.fibres <- t.fibres + 1;
    Fun.protect fn
      ~finally:(fun () ->
          t.fibres <- t.fibres - 1;
          if t.fibres = 0 then
            Waiters.wake_all t.waiter (Ok ())
        )

let await_idle t =
  while t.fibres > 0 do
    Ctf.note_try_read t.id;
    await t.waiter t.id
  done

let top fn =
  let id = Ctf.mint_id () in
  Ctf.note_created id Ctf.Switch;
  let q = Lwt_dllist.create () in
  let t = {
    id;
    state = On q;
    fibres = 0;
    waiter = Waiters.create ();
  } in
  match fn t with
  | v ->
    await_idle t;
    begin match t.state with
      | On _ ->
        (* Success. Just mark the switch as unusable now. *)
        turn_off t switch_finished;
        Ctf.note_read t.id;
        v
      | Off ex ->
        (* Function succeeded, but got failure waiting for fibres to finish. *)
        Ctf.note_read t.id;
        raise ex
    end
  | exception ex ->
    (* Main function failed.
       Turn the switch off to cancel any running fibres, if it's not off already. *)
    turn_off t ex;
    await_idle t;
    Ctf.note_read t.id;
    match t.state with
    | On _ -> assert false
    | Off ex -> raise ex        (* Raise the first exception the switch got. *)

let sub ~sw ~on_error fn =
  let w = ref ignore in
  match
    top (fun child ->
        w := add_cancel_hook sw (turn_off child);
        try fn child
        with ex -> turn_off child ex; raise ex
      )
  with
  | v ->
    Waiters.remove_waiter !w;
    v
  | exception ex ->
    Waiters.remove_waiter !w;
    on_error ex
