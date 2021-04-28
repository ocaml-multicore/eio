type 'a waiters = (('a, exn) result -> unit) Queue.t

type 'a state =
  | Unresolved of 'a waiters
  | Fulfilled of 'a
  | Broken of exn

type 'a t = {
  id : Ctf.id;
  mutable state : 'a state;
}

type 'a u = 'a t

effect Await : Ctf.id * 'a waiters -> 'a

let create ?label () =
  let id = Ctf.mint_id () in
  Ctf.note_created ?label id Ctf.Task;
  let t = { id; state = Unresolved (Queue.create ()) } in
  t, t

let fulfilled x =
  let id = Ctf.mint_id () in
  Ctf.note_created id Ctf.Task;
  { id; state = Fulfilled x }

let broken ex =
  let id = Ctf.mint_id () in
  Ctf.note_created id Ctf.Task;
  { id; state = Broken ex }

let await t =
  match t.state with
  | Fulfilled x ->
    Ctf.note_read t.id;
    x
  | Broken ex ->
    Ctf.note_read t.id;
    raise ex
  | Unresolved q ->
    Ctf.note_try_read t.id;
    perform (Await (t.id, q))

let await_result t =
  match await t with
  | x -> Ok x
  | exception ex -> Error ex

let fulfill t v =
  match t.state with
  | Broken ex -> invalid_arg ("Can't fulfill already-broken promise: " ^ Printexc.to_string ex)
  | Fulfilled _ -> invalid_arg "Can't fulfill already-fulfilled promise"
  | Unresolved q ->
    Ctf.note_resolved t.id ~ex:None;
    t.state <- Fulfilled v;
    Queue.iter (fun f -> f (Ok v)) q

let break t ex =
  match t.state with
  | Broken orig -> invalid_arg (Printf.sprintf "Can't break already-broken promise: %s -> %s"
                                  (Printexc.to_string orig) (Printexc.to_string ex))
  | Fulfilled _ -> invalid_arg (Printf.sprintf "Can't break already-fulfilled promise (with %s)"
                                  (Printexc.to_string ex))
  | Unresolved q ->
    Ctf.note_resolved t.id ~ex:(Some ex);
    t.state <- Broken ex;
    Queue.iter (fun f -> f (Error ex)) q

let resolve t = function
  | Ok x -> fulfill t x
  | Error ex -> break t ex

let state t = t.state
let id t = t.id

let is_resolved t =
  match t.state with
  | Fulfilled _ | Broken _ -> true
  | Unresolved _ -> false

let add_waiter waiters cb =
  Queue.add cb waiters
