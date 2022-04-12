type t = {
  waiters: unit Waiters.t; 
  mutex: Mutex.t;
  id: Ctf.id
}

let create () = {
  waiters = Waiters.create ();
  id = Ctf.mint_id ();
  mutex = Mutex.create ()
}

let await ?mutex t = 
  Mutex.lock t.mutex;
  Option.iter Eio_mutex.unlock mutex;
  Waiters.await ~mutex:(Some t.mutex) t.waiters t.id;
  Option.iter Eio_mutex.lock mutex

let broadcast t =
  Waiters.wake_all t.waiters ()
