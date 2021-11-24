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
  parent : t;
  children : t Lwt_dllist.t;
  protected : bool;
}

type fibre_context = {
  tid : Ctf.id;
  mutable cancel : t;
}

(* A dummy value for bootstrapping *)
let rec boot = {
  state = Finished;
  parent = boot;
  children = Lwt_dllist.create ();
  protected = false;
}

type _ eff += Get_context : fibre_context eff

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

(* Runs [fn] with a fresh cancellation context. *)
let with_cc ~ctx ~parent ~protected fn =
  let q = Lwt_dllist.create () in
  let children = Lwt_dllist.create () in
  let t = { state = On q; parent; children; protected } in
  let node = Lwt_dllist.add_r t parent.children in
  ctx.cancel <- t;
  match fn t with
  | x            -> ctx.cancel <- t.parent; t.state <- Finished; Lwt_dllist.remove node; x
  | exception ex -> ctx.cancel <- t.parent; t.state <- Finished; Lwt_dllist.remove node; raise ex

let protect fn =
  let ctx = perform Get_context in
  with_cc ~ctx ~parent:ctx.cancel ~protected:true @@ fun t ->
  let x = fn () in
  check t;
  x

let add_hook t hook =
  match t.state with
  | Finished -> invalid_arg "Cancellation context finished!"
  | Cancelling (ex, _) -> protect (fun () -> hook (Cancelled ex)); Hook.null
  | On q -> Hook.Node (Lwt_dllist.add_r hook q)

let rec cancel t ex =
  match t.state with
  | Finished -> invalid_arg "Cancellation context finished!"
  | Cancelling _ -> ()
  | On q ->
    let bt = Printexc.get_raw_backtrace () in
    t.state <- Cancelling (ex, bt);
    let cex = Cancelled ex in
    let rec aux () =
      match Lwt_dllist.take_opt_r q with
      | None -> Lwt_dllist.fold_r (cancel_child ex) t.children []
      | Some f ->
        match f cex with
        | () -> aux ()
        | exception ex2 -> ex2 :: aux ()
    in
    match protect aux with
    | [] -> ()
    | exns -> raise (Cancel_hook_failed exns)
and cancel_child ex t acc =
  if t.protected then acc
  else match cancel t ex with
    | () -> acc
    | exception ex -> ex :: acc

let sub fn =
  let ctx = perform Get_context in
  with_cc ~ctx ~parent:ctx.cancel ~protected:false @@ fun t ->
  let x =
    match fn t with
    | x ->
      check t.parent;
      x
    | exception ex ->
      check t.parent;
      raise ex
  in
  match t.state with
  | On _ -> x
  | Cancelling (ex, bt) -> Printexc.raise_with_backtrace ex bt
  | Finished -> invalid_arg "Cancellation context finished!"

(* Like [sub], but it's OK if the new context is cancelled.
   (instead, return the parent context on exit so the caller can check that) *)
let sub_unchecked fn =
  let ctx = perform Get_context in
  with_cc ~ctx ~parent:ctx.cancel ~protected:false @@ fun t ->
  fn t;
  t.parent
