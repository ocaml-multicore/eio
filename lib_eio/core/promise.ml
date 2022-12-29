type 'a state =
  | Resolved of 'a
  | Unresolved of Broadcast.t

type !'a promise = {
  id : Ctf.id;
  state : 'a state Atomic.t;    (* Note: we always switch to Resolved before broadcasting *)
}

type +!'a t
type -!'a u

type 'a or_exn = ('a, exn) result t

let to_public_promise : 'a promise -> 'a t = Obj.magic
let to_public_resolver : 'a promise -> 'a u = Obj.magic
let of_public_promise : 'a t -> 'a promise = Obj.magic
let of_public_resolver : 'a u -> 'a promise = Obj.magic

let create_with_id id =
  let t = {
    id;
    state = Atomic.make (Unresolved (Broadcast.create ()));
  } in
  to_public_promise t, to_public_resolver t

let create ?label () =
  let id = Ctf.mint_id () in
  Ctf.note_created ?label id Ctf.Promise;
  create_with_id id

let create_resolved x =
  let id = Ctf.mint_id () in
  Ctf.note_created id Ctf.Promise;
  to_public_promise { id; state = Atomic.make (Resolved x) }

let await t =
  let t = of_public_promise t in
  match Atomic.get t.state with
  | Resolved x ->
    Ctf.note_read t.id;
    x
  | Unresolved b ->
    Suspend.enter (fun ctx enqueue ->
        match Broadcast.suspend b (fun () -> enqueue (Ok ())) with
        | None -> ()  (* We got resumed immediately *)
        | Some request ->
          match Atomic.get t.state with
          | Resolved _ ->
            (* The promise was resolved as we were suspending.
               Resume now if we haven't already done so. *)
            if Broadcast.cancel request then enqueue (Ok ())
          | Unresolved _ ->
            (* We observed the promise to be still unresolved after registering a waiter.
               Therefore any resolution must happen after we were registered and we will be notified. *)
            Ctf.note_try_read t.id;
            Cancel.Fiber_context.set_cancel_fn ctx (fun ex ->
                if Broadcast.cancel request then enqueue (Error ex)
                (* else already resumed *)
              )
      );
    match Atomic.get t.state with
    | Resolved x ->
      Ctf.note_read t.id;
      x
    | Unresolved _ -> assert false

let await_exn t =
  match await t with
  | Ok x -> x
  | Error ex -> raise ex

let resolve t v =
  let rec resolve' t v =
    match Atomic.get t.state with
    | Resolved _ -> invalid_arg "Can't resolve already-resolved promise"
    | Unresolved b as prev ->
      if Atomic.compare_and_set t.state prev (Resolved v) then (
        Ctf.note_resolved t.id ~ex:None;
        Broadcast.resume_all b
      ) else (
        (* Otherwise, the promise was already resolved. Retry (to get the error). *)
        resolve' t v
      )
  in
  resolve' (of_public_resolver t) v

let resolve_ok    u x = resolve u (Ok x)
let resolve_error u x = resolve u (Error x)

let peek t =
  let t = of_public_promise t in
  match Atomic.get t.state with
  | Unresolved _ -> None
  | Resolved x -> Some x

let id t =
  let t = of_public_promise t in
  t.id

let is_resolved t =
  Option.is_some (peek t)
