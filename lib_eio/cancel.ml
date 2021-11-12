open EffectHandlers

exception Cancel_hook_failed of exn list

exception Cancelled of exn

let () =
  Printexc.register_printer @@ function
  | Cancel_hook_failed exns -> Some ("During cancellation:\n" ^ String.concat "\nand\n" (List.map Printexc.to_string exns))
  | Cancelled ex -> Some ("Cancelled: " ^ Printexc.to_string ex)
  | _ -> None

type state =
  | On of (exn -> unit) Lwt_dllist.t
  | Cancelling of exn * Printexc.raw_backtrace
  | Finished

type t = {
  mutable state : state;
}

(* A dummy value for bootstrapping *)
let boot = {
  state = Finished;
}

type _ eff += Set_cancel : t -> t eff

let cancelled t =
  match t.state with
  | On _ -> false
  | Cancelling _ -> true
  | Finished -> invalid_arg "Cancellation context finished!"

let check t =
  match t.state with
  | On _ -> ()
  | Cancelling (ex, _) -> raise (Cancelled ex)
  | Finished -> invalid_arg "Cancellation context finished!"

let get_error t =
  match t.state with
  | On _ -> None
  | Cancelling (ex, _) -> Some (Cancelled ex)
  | Finished -> Some (Invalid_argument "Cancellation context finished!")

let is_finished t =
  match t.state with
  | Finished -> true
  | On _ | Cancelling _ -> false

(* Runs [fn] with a fresh cancellation context value (but does not install it). *)
let with_cc fn =
  let q = Lwt_dllist.create () in
  let t = { state = On q } in
  Fun.protect (fun () -> fn t)
    ~finally:(fun () -> t.state <- Finished)

let protect_full fn =
  with_cc @@ fun t ->
  let x =
    let old = perform (Set_cancel t) in
    Fun.protect (fun () -> fn t)
      ~finally:(fun () -> ignore (perform (Set_cancel old)))
  in
  check t;
  x

let protect fn = protect_full (fun (_ : t) -> fn ())

let add_hook_unwrapped t hook =
  match t.state with
  | Finished -> invalid_arg "Cancellation context finished!"
  | Cancelling (ex, _) -> protect (fun () -> hook ex); Hook.null
  | On q ->
    let node = Lwt_dllist.add_r hook q in
    (fun () -> Lwt_dllist.remove node)

let add_hook t hook = add_hook_unwrapped t (fun ex -> hook (Cancelled ex))

let cancel t ex =
  match t.state with
  | Finished -> invalid_arg "Cancellation context finished!"
  | Cancelling _ -> ()
  | On q ->
    let bt = Printexc.get_raw_backtrace () in
    t.state <- Cancelling (ex, bt);
    let rec aux () =
      match Lwt_dllist.take_opt_r q with
      | None -> []
      | Some f ->
        match f ex with
        | () -> aux ()
        | exception ex2 -> ex2 :: aux ()
    in
    match protect aux with
    | [] -> ()
    | exns -> raise (Cancel_hook_failed exns)

let sub fn =
  with_cc @@ fun t ->
  let x =
    (* Can't use Fun.protect here because of [Fun.Finally_raised]. *)
    let old = perform (Set_cancel t) in
    match 
      let unhook = add_hook_unwrapped old (cancel t) in
      Fun.protect (fun () -> fn t) ~finally:unhook
    with
    | x ->
      ignore (perform (Set_cancel old));
      check old;
      x
    | exception ex ->
      ignore (perform (Set_cancel old));
      check old;
      raise ex
  in
  match t.state with
  | On _ -> x
  | Cancelling (ex, bt) -> Printexc.raise_with_backtrace ex bt
  | Finished -> invalid_arg "Cancellation context finished!"

(* Like [sub], but it's OK if the new context is cancelled.
   (instead, return the parent context on exit so the caller can check that) *)
let sub_unchecked fn =
  with_cc @@ fun t ->
  let old = perform (Set_cancel t) in
  Fun.protect (fun () ->
      let unhook = add_hook_unwrapped old (cancel t) in
      Fun.protect (fun () -> fn t) ~finally:unhook
    )
    ~finally:(fun () -> ignore (perform (Set_cancel old)));
  old
