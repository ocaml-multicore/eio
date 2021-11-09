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

let with_t fn =
  let q = Lwt_dllist.create () in
  let t = { state = On q } in
  Fun.protect (fun () -> fn t)
    ~finally:(fun () -> t.state <- Finished)

let protect_full fn =
  with_t @@ fun t ->
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
  with_t @@ fun t ->
  let x =
    let old = perform (Set_cancel t) in
    Fun.protect (fun () ->
        let unhook = add_hook_unwrapped old (cancel t) in
        Fun.protect (fun () -> fn t) ~finally:unhook
      )
      ~finally:(fun () -> ignore (perform (Set_cancel old)))
  in
  check t;
  x
