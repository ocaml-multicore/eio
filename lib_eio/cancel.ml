open Effect

exception Cancelled = Exn.Cancelled
exception Cancel_hook_failed = Exn.Cancel_hook_failed

type state =
  | On
  | Cancelling of exn * Printexc.raw_backtrace
  | Finished

(* There is a tree of cancellation contexts for each domain.
   A fibre is always in exactly one context, but can move to a new child and back (see [sub]).
   While a fibre is performing a cancellable operation, it sets a cancel function.
   When a context is cancelled, we attempt to call and remove each fibre's cancellation function, if any.
   Cancelling always happens from the fibre's own domain, but the cancellation function may be removed
   from another domain as soon as an operation is known to have succeeded.
   An operation may either finish normally or be cancelled;
   whoever manages to clear the cancellation function is responsible for resuming the continuation.
   If cancelled, this is done by calling the cancellation function. *)
type t = {
  mutable state : state;
  children : t Lwt_dllist.t;
  fibres : fibre_context Lwt_dllist.t;
  protected : bool;
  domain : Domain.id;         (* Prevent access from other domains *)
}
and fibre_context = {
  tid : Ctf.id;
  mutable cancel_context : t;
  mutable cancel_node : fibre_context Lwt_dllist.node option; (* Our entry in [cancel_context.fibres] *)
  cancel_fn : (exn -> unit) option Atomic.t;
}

type _ eff += Get_context : fibre_context eff

let pp_state f t =
  begin match t.state with
    | On -> Fmt.string f "on"
    | Cancelling (ex, _) -> Fmt.pf f "cancelling(%a)" Fmt.exn ex
    | Finished -> Fmt.string f "finished"
  end;
  if t.protected then Fmt.pf f " (protected)"

let pp_fibre f fibre =
  Fmt.pf f "%d" (fibre.tid :> int)

let pp_lwt_dlist ~sep pp f t =
  let first = ref true in
  t |> Lwt_dllist.iter_l (fun item ->
      if !first then first := false
      else sep f ();
      pp f item;
    )

let rec dump f t =
  Fmt.pf f "@[<v2>%a [%a]%a@]"
    pp_state t
    (pp_lwt_dlist ~sep:(Fmt.any ",") pp_fibre) t.fibres
    pp_children t.children
and pp_children f ts =
  ts |> Lwt_dllist.iter_l (fun t ->
      Fmt.cut f ();
      dump f t
    )

let is_on t =
  match t.state with
  | On -> true
  | Cancelling _ | Finished -> false

let check t =
  match t.state with
  | On -> ()
  | Cancelling (ex, _) -> raise (Cancelled ex)
  | Finished -> invalid_arg "Cancellation context finished!"

let get_error t =
  match t.state with
  | On -> None
  | Cancelling (ex, _) -> Some (Cancelled ex)
  | Finished -> Some (Invalid_argument "Cancellation context finished!")

let is_finished t =
  match t.state with
  | Finished -> true
  | On | Cancelling _ -> false

let move_fibre_to t fibre =
  let new_node = Lwt_dllist.add_r fibre t.fibres in     (* Add to new context *)
  fibre.cancel_context <- t;
  Option.iter Lwt_dllist.remove fibre.cancel_node;      (* Remove from old context *)
  fibre.cancel_node <- Some new_node

(* Note: the new value is not linked into the cancellation tree. *)
let create ~protected =
  let children = Lwt_dllist.create () in
  let fibres = Lwt_dllist.create () in
  { state = Finished; children; protected; fibres; domain = Domain.self () }

(* Links [t] into the tree as a child of [parent] and returns a function to remove it again. *)
let activate t ~parent =
  assert (t.state = Finished);
  assert (parent.state <> Finished);
  t.state <- On;
  let node = Lwt_dllist.add_r t parent.children in
  fun () ->
    assert (parent.state <> Finished);
    t.state <- Finished;
    Lwt_dllist.remove node

(* Runs [fn] with a fresh cancellation context. *)
let with_cc ~ctx:fibre ~parent ~protected fn =
  let t = create ~protected in
  let deactivate = activate t ~parent in
  move_fibre_to t fibre;
  let cleanup () = move_fibre_to parent fibre; deactivate () in
  match fn t with
  | x            -> cleanup (); x
  | exception ex -> cleanup (); raise ex

let protect fn =
  let ctx = perform Get_context in
  with_cc ~ctx ~parent:ctx.cancel_context ~protected:true @@ fun _ ->
  (* Note: there is no need to check the new context after [fn] returns;
     the goal of cancellation is only to finish the thread promptly, not to report the error.
     We also do not check the parent context, to make sure the caller has a chance to handle the result. *)
  fn ()

let rec cancel_internal t ex acc_fns =
  let collect_cancel_fn fibre acc =
    match Atomic.exchange fibre.cancel_fn None with
    | None -> acc        (* The operation succeeded and so can't be cancelled now *)
    | Some cancel_fn -> cancel_fn :: acc
  in
  match t.state with
  | Finished -> invalid_arg "Cancellation context finished!"
  | Cancelling _ -> acc_fns
  | On ->
    let bt = Printexc.get_raw_backtrace () in
    t.state <- Cancelling (ex, bt);
    let acc_fns = Lwt_dllist.fold_l collect_cancel_fn t.fibres acc_fns in
    Lwt_dllist.fold_r (cancel_child ex) t.children acc_fns
and cancel_child ex t acc =
  if t.protected then acc
  else cancel_internal t ex acc

let check_our_domain t =
  if Domain.self () <> t.domain then invalid_arg "Cancellation context accessed from wrong domain!"

let cancel t ex =
  check_our_domain t;
  let fns = cancel_internal t ex [] in
  let cex = Cancelled ex in
  let rec aux = function
    | [] -> []
    | fn :: fns ->
      match fn cex with
      | () -> aux fns
      | exception ex2 -> ex2 :: aux fns
  in
  if fns <> [] then (
    match protect (fun () -> aux fns) with
    | [] -> ()
    | exns -> raise (Cancel_hook_failed exns)
  )

let sub fn =
  let ctx = perform Get_context in
  let parent = ctx.cancel_context in
  with_cc ~ctx ~parent ~protected:false @@ fun t ->
  fn t

(* Like [sub], but it's OK if the new context is cancelled.
   (instead, return the parent context on exit so the caller can check that) *)
let sub_unchecked fn =
  let ctx = perform Get_context in
  let parent = ctx.cancel_context in
  with_cc ~ctx ~parent ~protected:false @@ fun t ->
  fn t;
  parent

module Fibre_context = struct
  type t = fibre_context

  let tid t = t.tid
  let cancellation_context t = t.cancel_context

  let get_error t = get_error t.cancel_context

  let set_cancel_fn t fn =
    (* if Atomic.exchange t.cancel_fn (Some fn) <> None then failwith "Fibre already has a cancel function!" *)
    Atomic.set t.cancel_fn (Some fn)

  let clear_cancel_fn t =
    Atomic.exchange t.cancel_fn None <> None

  let make ~cc =
    let tid = Ctf.mint_id () in
    Ctf.note_created tid Ctf.Task;
    let t = { tid; cancel_context = cc; cancel_node = None; cancel_fn = Atomic.make None } in
    t.cancel_node <- Some (Lwt_dllist.add_r t cc.fibres);
    t

  let make_root () =
    let cc = create ~protected:false in
    cc.state <- On;
    make ~cc

  let destroy t =
    Option.iter Lwt_dllist.remove t.cancel_node
end
