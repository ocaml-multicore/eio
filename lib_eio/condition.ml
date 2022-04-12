type 'a or_exn = V of 'a | Exn of exn

type 'a t = {
  waiters: 'a or_exn Waiters.t; 
  id: Ctf.id
}

let create () = {
  waiters = Waiters.create ();
  id = Ctf.mint_id ()}

let wait ?mutex t = 
  Option.iter Eio_mutex.unlock mutex;
  let res = Waiters.await ~mutex:None t.waiters t.id in
  Option.iter Eio_mutex.lock mutex;
  match res with
  | V v -> v
  | Exn exn -> raise exn

let signal t v = 
  Waiters.wake_one t.waiters (V v) |> ignore

let broadcast t v =
  Waiters.wake_all t.waiters (V v)

let broadcast_exn t v =
  Waiters.wake_all t.waiters (Exn v)
