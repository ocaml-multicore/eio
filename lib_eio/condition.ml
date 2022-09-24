type t = {
  waiters: unit Waiters.t;
  mutex: Mutex.t;
  id: Ctf.id
}

let create () = {
  waiters = Waiters.create ();
  id = Ctf.mint_id ();
  mutex = Mutex.create ();
}

let await t mutex =
  Mutex.lock t.mutex;
  Eio_mutex.unlock mutex;
  match Waiters.await ~mutex:(Some t.mutex) t.waiters t.id with
  | ()           -> Eio_mutex.lock mutex
  | exception ex -> Eio_mutex.lock mutex; raise ex

let await_no_mutex t =
  Mutex.lock t.mutex;
  Waiters.await ~mutex:(Some t.mutex) t.waiters t.id

let broadcast t =
  Mutex.lock t.mutex;
  Waiters.wake_all t.waiters ();
  Mutex.unlock t.mutex
