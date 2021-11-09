type state =
  | Free of int
  | Waiting of unit Waiters.t

type t = {
  id : Ctf.id;
  mutable state : state;
}

let make n =
  if n < 0 then raise (Invalid_argument "n < 0");
  let id = Ctf.mint_id () in
  Ctf.note_created id Ctf.Semaphore;
  {
    id;
    state = Free n;
  }

let release t =
  Ctf.note_signal t.id;
  match t.state with
  | Free x when x = max_int -> raise (Sys_error "semaphore would overflow max_int!")
  | Free x -> t.state <- Free (succ x)
  | Waiting q ->
    match Waiters.wake_one q (Ok ()) with
    | `Ok -> ()
    | `Queue_empty ->
      t.state <- Free 1

let rec acquire t =
  match t.state with
  | Waiting q ->
    Ctf.note_try_read t.id;
    Switch.await q t.id |> Switch.or_raise
  | Free 0 ->
    t.state <- Waiting (Waiters.create ());
    acquire t
  | Free n ->
    Ctf.note_read t.id;
    t.state <- Free (pred n)

let get_value t =
  match t.state with
  | Free n -> n
  | Waiting _ -> 0
