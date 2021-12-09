type state =
  | Free of int
  | Waiting of unit Waiters.t

type t = {
  id : Ctf.id;
  mutex : Mutex.t;
  mutable state : state;
}

let make n =
  if n < 0 then raise (Invalid_argument "n < 0");
  let id = Ctf.mint_id () in
  Ctf.note_created id Ctf.Semaphore;
  {
    id;
    mutex = Mutex.create ();
    state = Free n;
  }

let release t =
  Mutex.lock t.mutex;
  Ctf.note_signal t.id;
  match t.state with
  | Free x when x = max_int -> Mutex.unlock t.mutex; raise (Sys_error "semaphore would overflow max_int!")
  | Free x -> t.state <- Free (succ x); Mutex.unlock t.mutex
  | Waiting q ->
    begin match Waiters.wake_one q () with
      | `Ok -> ()
      | `Queue_empty -> t.state <- Free 1
    end;
    Mutex.unlock t.mutex

let rec acquire t =
  Mutex.lock t.mutex;
  match t.state with
  | Waiting q ->
    Ctf.note_try_read t.id;
    Waiters.await ~mutex:(Some t.mutex) q t.id
  | Free 0 ->
    t.state <- Waiting (Waiters.create ());
    Mutex.unlock t.mutex;
    acquire t
  | Free n ->
    Ctf.note_read t.id;
    t.state <- Free (pred n);
    Mutex.unlock t.mutex

let get_value t =
  Mutex.lock t.mutex;
  let s = t.state in
  Mutex.unlock t.mutex;
  match s with
  | Free n -> n
  | Waiting _ -> 0
