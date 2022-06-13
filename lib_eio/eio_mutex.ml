type state =
  | Unlocked                    (* can be locked *)
  | Locked                      (* is locked; threads may be waiting *)
  | Poisoned of exn             (* disabled due to exception in critical section *)

exception Poisoned of exn

type t = {
  id : Ctf.id;
  mutex : Mutex.t;
  mutable state : state;
  waiters : unit Waiters.t;
}

let create () =
  let id = Ctf.mint_id () in
  Ctf.note_created id Ctf.Mutex;
  {
    id;
    mutex = Mutex.create ();
    state = Unlocked;
    waiters = Waiters.create ();
  }

let unlock t =
  Mutex.lock t.mutex;
  Ctf.note_signal t.id;
  match t.state with
  | Unlocked -> 
    Mutex.unlock t.mutex;
    raise (Sys_error "Eio.Mutex.unlock: already unlocked!")
  | Locked ->
    begin match Waiters.wake_one t.waiters () with
      | `Ok -> ()
      | `Queue_empty -> t.state <- Unlocked
    end;
    Mutex.unlock t.mutex
  | Poisoned ex ->
    Mutex.unlock t.mutex;
    raise (Poisoned ex)

let lock t =
  Mutex.lock t.mutex;
  match t.state with
  | Locked ->
    Ctf.note_try_read t.id;
    Waiters.await ~mutex:(Some t.mutex) t.waiters t.id
    (* The unlocker didn't change the state, so it's still locked, as required. *)
  | Unlocked -> 
    Ctf.note_read t.id;
    t.state <- Locked;
    Mutex.unlock t.mutex
  | Poisoned ex ->
    Mutex.unlock t.mutex;
    raise (Poisoned ex)

let try_lock t =
  Mutex.lock t.mutex;
  match t.state with
  | Locked ->
    Ctf.note_try_read t.id;
    Mutex.unlock t.mutex;
    false
  | Unlocked -> 
    Ctf.note_read t.id;
    t.state <- Locked;
    Mutex.unlock t.mutex;
    true
  | Poisoned ex ->
    Mutex.unlock t.mutex;
    raise (Poisoned ex)

let poison t ex =
  Mutex.lock t.mutex;
  t.state <- Poisoned ex;
  Mutex.unlock t.mutex

let with_lock ?(on_exn=`Poison) t fn =
  lock t;
  match fn () with
  | x -> unlock t; x
  | exception ex ->
    match on_exn with
    | `Unlock -> unlock t; raise ex
    | `Poison -> poison t ex; raise ex
