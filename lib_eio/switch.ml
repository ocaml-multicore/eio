open EffectHandlers

exception Multiple_exceptions of exn list

exception Cancelled of exn

type hook = unit -> unit                (* A function to remove the hook *)

let () =
  Printexc.register_printer @@ function
  | Multiple_exceptions exns -> Some ("Multiple exceptions:\n" ^ String.concat "\nand\n" (List.map Printexc.to_string exns))
  | Cancelled ex -> Some ("Cancelled: " ^ Printexc.to_string ex)
  | _ -> None

type state =
  | On of (exn -> unit) Lwt_dllist.t
  | Off of exn * Printexc.raw_backtrace
  | Finished

type t = {
  id : Ctf.id;
  mutable state : state;
  mutable fibres : int;
  mutable extra_exceptions : exn list;
  on_release : (unit -> unit) Lwt_dllist.t;
  waiter : unit Waiters.t;              (* The main [top]/[sub] function may wait here for fibres to finish. *)
}

(* A dummy switch for bootstrapping *)
let boot_switch = {
  id = Ctf.mint_id ();
  state = Finished;
  fibres = 0;
  extra_exceptions = [];
  on_release = Lwt_dllist.create ();
  waiter = Waiters.create ();
}

type _ eff += Set_switch : t -> t eff

let with_switch t fn =
  let old = perform (Set_switch t) in
  Fun.protect fn
    ~finally:(fun () -> ignore (perform (Set_switch old)))

let null_hook = ignore

let remove_hook h = h ()

let check t =
  match t.state with
  | On _ -> ()
  | Off (ex, _) -> raise (Cancelled ex)
  | Finished -> invalid_arg "Switch finished!"

let get_error t =
  match t.state with
  | On _ -> None
  | Off (ex, _) -> Some (Cancelled ex)
  | Finished -> Some (Invalid_argument "Switch finished!")

let is_finished t =
  match t.state with
  | Finished -> true
  | On _ | Off _ -> false

let rec turn_off t ex =
  match t.state with
  | Finished -> invalid_arg "Switch finished!"
  | Off (orig, _) when orig == ex || List.memq ex t.extra_exceptions -> ()
  | Off _ ->
    begin match ex with
      | Cancelled _ -> ()       (* The original exception will be reported elsewhere *)
      | Multiple_exceptions exns -> List.iter (turn_off t) exns
      | _ -> t.extra_exceptions <- ex :: t.extra_exceptions
    end
  | On q ->
    Ctf.note_resolved t.id ~ex:(Some ex);
    t.state <- Off (ex, Printexc.get_raw_backtrace ());
    let rec aux () =
      match Lwt_dllist.take_opt_r q with
      | None -> ()
      | Some f ->
        begin
          try f ex 
          with ex2 -> turn_off t ex2
        end;
        aux ()
    in
    aux ()

let add_cancel_hook t hook =
  match t.state with
  | Finished -> invalid_arg "Switch finished!"
  | Off (ex, _) -> hook ex; ignore
  | On q ->
    let node = Lwt_dllist.add_r hook q in
    (fun () -> Lwt_dllist.remove node)

let add_cancel_hook_opt t hook =
  match t with
  | Some t -> add_cancel_hook t hook
  | None -> ignore

let with_op t fn =
  check t;
  t.fibres <- t.fibres + 1;
  Fun.protect fn
    ~finally:(fun () ->
        t.fibres <- t.fibres - 1;
        if t.fibres = 0 then
          Waiters.wake_all t.waiter (Ok ())
      )

let await_internal ?sw waiters id tid enqueue =
  let cleanup_hooks = Queue.create () in
  let when_resolved r =
    Queue.iter Waiters.remove_waiter cleanup_hooks;
    Ctf.note_read ~reader:id tid;
    enqueue r
  in
  let cancel ex = when_resolved (Error ex) in
  sw |> Option.iter (fun sw ->
      let cancel_waiter = add_cancel_hook sw cancel in
      Queue.add cancel_waiter cleanup_hooks;
    );
  let resolved_waiter = Waiters.add_waiter waiters when_resolved in
  Queue.add resolved_waiter cleanup_hooks

let await ?sw waiters id =
  Suspend.enter_unchecked (await_internal ?sw waiters id)

let rec await_idle t =
  (* Wait for fibres to finish: *)
  while t.fibres > 0 do
    Ctf.note_try_read t.id;
    await t.waiter t.id
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

let raise_with_extras t ex bt =
  match t.extra_exceptions with
  | [] -> Printexc.raise_with_backtrace ex bt
  | exns -> Printexc.raise_with_backtrace (Multiple_exceptions (ex :: List.rev exns)) bt

let top fn =
  let id = Ctf.mint_id () in
  Ctf.note_created id Ctf.Switch;
  let q = Lwt_dllist.create () in
  let t = {
    id;
    state = On q;
    fibres = 0;
    extra_exceptions = [];
    waiter = Waiters.create ();
    on_release = Lwt_dllist.create ();
  } in
  with_switch t @@ fun () ->
  match fn t with
  | v ->
    await_idle t;
    begin match t.state with
      | Finished -> assert false
      | On _ ->
        (* Success. Just mark the switch as unusable now. *)
        t.state <- Finished;
        Ctf.note_read t.id;
        v
      | Off (ex, bt) ->
        (* Function succeeded, but got failure waiting for fibres to finish. *)
        t.state <- Finished;
        Ctf.note_read t.id;
        raise_with_extras t ex bt
    end
  | exception ex ->
    (* Main function failed.
       Turn the switch off to cancel any running fibres, if it's not off already. *)
    turn_off t ex;
    await_idle t;
    Ctf.note_read t.id;
    match t.state with
    | On _ | Finished -> assert false
    | Off (ex, bt) ->
      t.state <- Finished;
      raise_with_extras t ex bt

let on_release_cancellable t fn =
  match t.state with
  | Finished ->
    fn ();
    invalid_arg "Switch finished!"
  | On _ | Off _ ->
    let node = Lwt_dllist.add_r fn t.on_release in
    (fun () -> Lwt_dllist.remove node)

let on_release t fn =
  match t.state with
  | Finished ->
    fn ();
    invalid_arg "Switch finished!"
  | On _ | Off _ ->
    let _ : _ Lwt_dllist.node = Lwt_dllist.add_r fn t.on_release in
    ()

let sub ?on_release:release sw ~on_error fn =
  match sw.state with
  | Finished ->
    (* Can't create child switch. Run release hooks immediately. *)
    Option.iter (fun f -> f ()) release;
    invalid_arg "Switch finished!"
  | Off (ex, _) ->
    (* Can't create child switch. Run release hooks immediately. *)
    Option.iter (fun f -> f ()) release;
    raise (Cancelled ex)
  | On _ ->
    with_op sw @@ fun () ->
    let w = ref ignore in
    match
      top (fun child ->
          w := add_cancel_hook sw (turn_off child);
          Option.iter (on_release child) release;
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

let sub_opt ?on_release:release t fn =
  match t with
  | Some t -> sub ?on_release:release ~on_error:raise t fn
  | None ->
    top (fun child ->
        Option.iter (on_release child) release;
        fn child
      )
