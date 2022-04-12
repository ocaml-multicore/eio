type state =
  | Unlocked                    (* can be locked *)
  | Locked                      (* is locked, no threads waiting *)
  | Waiting of unit Waiters.t   (* is locked, threads waiting *)

type t = {
  id : Ctf.id;
  mutex : Mutex.t;
  mutable state : state;
}

let create () =
  let id = Ctf.mint_id () in
  Ctf.note_created id Ctf.Mutex;
  {
    id;
    mutex = Mutex.create ();
    state = Unlocked;
  }

let is_locked t =
  Mutex.lock t.mutex;
  let s = t.state in
  Mutex.unlock t.mutex;
  match s with
  | Unlocked -> false
  | Locked | Waiting _ -> true

exception Already_unlocked

let unlock t =
  Mutex.lock t.mutex;
  Ctf.note_signal t.id;
  match t.state with
  | Unlocked -> 
    Mutex.unlock t.mutex;
    raise Already_unlocked
  | Locked ->
    t.state <- Unlocked;
    Mutex.unlock t.mutex
  | Waiting q ->
    begin match Waiters.wake_one q () with
      | `Ok -> ()
      | `Queue_empty -> t.state <- Unlocked
    end;
    Mutex.unlock t.mutex

let rec lock t =
  Mutex.lock t.mutex;
  match t.state with
  | Waiting q ->
    Ctf.note_try_read t.id;
    Waiters.await ~mutex:(Some t.mutex) q t.id
  | Locked ->
    t.state <- Waiting (Waiters.create ());
    Mutex.unlock t.mutex;
    lock t
  | Unlocked -> 
    Ctf.note_read t.id;
    t.state <- Locked;
    Mutex.unlock t.mutex

let with_lock t fn =
  lock t;
  Fun.protect ~finally:(fun () -> unlock t) fn
