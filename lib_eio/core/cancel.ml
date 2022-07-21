exception Cancelled = Exn.Cancelled
exception Cancel_hook_failed = Exn.Cancel_hook_failed

type state =
  | On
  | Cancelling of exn * Printexc.raw_backtrace
  | Finished

(* There is a tree of cancellation contexts for each domain.
   A fiber is always in exactly one context, but can move to a new child and back (see [sub]).
   While a fiber is performing a cancellable operation, it sets a cancel function.
   When a context is cancelled, we attempt to call and remove each fiber's cancellation function, if any.
   Cancelling always happens from the fiber's own domain, but the cancellation function may be removed
   from another domain as soon as an operation is known to have succeeded.
   An operation may either finish normally or be cancelled;
   whoever manages to clear the cancellation function is responsible for resuming the continuation.
   If cancelled, this is done by calling the cancellation function. *)
type t = {
  mutable state : state;
  children : t Lwt_dllist.t;
  fibers : fiber_context Lwt_dllist.t;
  protected : bool;
  domain : Domain.id;         (* Prevent access from other domains *)
}
and fiber_context = {
  tid : Ctf.id;
  mutable cancel_context : t;
  mutable cancel_node : fiber_context Lwt_dllist.node option; (* Our entry in [cancel_context.fibers] *)
  cancel_fn : (exn -> unit) option Atomic.t;
  mutable vars : Hmap.t;
}

type _ Effect.t += Get_context : fiber_context Effect.t

let pp_state f t =
  begin match t.state with
    | On -> Fmt.string f "on"
    | Cancelling (ex, _) -> Fmt.pf f "cancelling(%a)" Fmt.exn ex
    | Finished -> Fmt.string f "finished"
  end;
  if t.protected then Fmt.pf f " (protected)"

let pp_fiber f fiber =
  Fmt.pf f "%d" (fiber.tid :> int)

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
    (pp_lwt_dlist ~sep:(Fmt.any ",") pp_fiber) t.fibers
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

let move_fiber_to t fiber =
  let new_node = Lwt_dllist.add_r fiber t.fibers in     (* Add to new context *)
  fiber.cancel_context <- t;
  Option.iter Lwt_dllist.remove fiber.cancel_node;      (* Remove from old context *)
  fiber.cancel_node <- Some new_node

(* Note: the new value is not linked into the cancellation tree. *)
let create ~protected =
  let children = Lwt_dllist.create () in
  let fibers = Lwt_dllist.create () in
  { state = Finished; children; protected; fibers; domain = Domain.self () }

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
let with_cc ~ctx:fiber ~parent ~protected fn =
  if not protected then check parent;
  let t = create ~protected in
  let deactivate = activate t ~parent in
  move_fiber_to t fiber;
  let cleanup () = move_fiber_to parent fiber; deactivate () in
  match fn t with
  | x            -> cleanup (); x
  | exception ex -> cleanup (); raise ex

let protect fn =
  let ctx = Effect.perform Get_context in
  with_cc ~ctx ~parent:ctx.cancel_context ~protected:true @@ fun _ ->
  (* Note: there is no need to check the new context after [fn] returns;
     the goal of cancellation is only to finish the thread promptly, not to report the error.
     We also do not check the parent context, to make sure the caller has a chance to handle the result. *)
  fn ()

let rec cancel_internal t ex acc_fns =
  let collect_cancel_fn fiber acc =
    match Atomic.exchange fiber.cancel_fn None with
    | None -> acc        (* The operation succeeded and so can't be cancelled now *)
    | Some cancel_fn -> cancel_fn :: acc
  in
  match t.state with
  | Finished -> invalid_arg "Cancellation context finished!"
  | Cancelling _ -> acc_fns
  | On ->
    let bt = Printexc.get_raw_backtrace () in
    t.state <- Cancelling (ex, bt);
    let acc_fns = Lwt_dllist.fold_r collect_cancel_fn t.fibers acc_fns in
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
  let ctx = Effect.perform Get_context in
  let parent = ctx.cancel_context in
  with_cc ~ctx ~parent ~protected:false @@ fun t ->
  fn t

(* Like [sub], but it's OK if the new context is cancelled.
   (instead, return the parent context on exit so the caller can check that) *)
let sub_unchecked fn =
  let ctx = Effect.perform Get_context in
  let parent = ctx.cancel_context in
  with_cc ~ctx ~parent ~protected:false @@ fun t ->
  fn t;
  parent

module Fiber_context = struct
  type t = fiber_context
  type vars = Hmap.t

  let tid t = t.tid
  let cancellation_context t = t.cancel_context

  let get_error t = get_error t.cancel_context

  let set_cancel_fn t fn =
    (* if Atomic.exchange t.cancel_fn (Some fn) <> None then failwith "Fiber already has a cancel function!" *)
    Atomic.set t.cancel_fn (Some fn)

  let clear_cancel_fn t =
    Atomic.exchange t.cancel_fn None <> None

  let make ~cc ~vars =
    let tid = Ctf.mint_id () in
    Ctf.note_created tid Ctf.Task;
    let t = { tid; cancel_context = cc; cancel_node = None; cancel_fn = Atomic.make None; vars } in
    t.cancel_node <- Some (Lwt_dllist.add_r t cc.fibers);
    t

  let make_root () =
    let cc = create ~protected:false in
    cc.state <- On;
    make ~cc ~vars:Hmap.empty

  let destroy t =
    Option.iter Lwt_dllist.remove t.cancel_node

  let vars t = t.vars

  let get_vars () =
    vars (Effect.perform Get_context)

  let with_vars t vars fn =
    let old_vars = t.vars in
    t.vars <- vars;
    let cleanup () = t.vars <- old_vars in
    match fn () with
    | x            -> cleanup (); x
    | exception ex -> cleanup (); raise ex
end
