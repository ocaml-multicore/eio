type 'a waiters = (('a, exn) result -> unit) Queue.t

type 'a state =
  | Unresolved of 'a waiters
  | Fulfilled of 'a
  | Broken of exn

type 'a t = {
  mutable state : 'a state;
}

type 'a u = 'a t

effect Await : 'a waiters -> 'a

let create () =
  let state = Unresolved (Queue.create ()) in
  let t = { state } in
  t, t

let await t =
  match t.state with
  | Fulfilled x -> x
  | Broken ex -> raise ex
  | Unresolved q ->
    perform (Await q)

let fulfill t v =
  match t.state with
  | Broken ex -> invalid_arg ("Can't fulfill already-broken promise: " ^ Printexc.to_string ex)
  | Fulfilled _ -> invalid_arg "Can't fulfill already-fulfilled promise"
  | Unresolved q ->
    t.state <- Fulfilled v;
    Queue.iter (fun f -> f (Ok v)) q

let break t ex =
  match t.state with
  | Broken orig -> invalid_arg (Printf.sprintf "Can't break already-broken promise: %s -> %s"
                                  (Printexc.to_string orig) (Printexc.to_string ex))
  | Fulfilled _ -> invalid_arg (Printf.sprintf "Can't break already-fulfilled promise (with %s)"
                                  (Printexc.to_string ex))
  | Unresolved q ->
    t.state <- Broken ex;
    Queue.iter (fun f -> f (Error ex)) q

let state t = t.state

let add_waiter waiters cb =
  Queue.add cb waiters
