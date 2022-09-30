type 'a t = {
  writers: unit Waiters.t;
  readers: unit Waiters.t;
  mutex: Mutex.t;
  id: Ctf.id;
  value: 'a option ref;
}

let create () = {
  writers = Waiters.create ();
  readers = Waiters.create ();
  mutex = Mutex.create ();
  id = Ctf.mint_id ();
  value = ref None;
}

let send (t:'a t) (m : 'a) : unit =
  let sent = ref false in
  let wait () =
    Mutex.lock t.mutex;
    match !(t.value) with
    | Some _ ->
      Waiters.await ~mutex:(Some t.mutex) t.writers t.id
    | None ->
      t.value := Some m;
      Waiters.wake_one t.readers () |> ignore;
      Mutex.unlock t.mutex;
      sent := true
  in
  while !sent = false do
    wait ()
  done

let recv t =
  let received = ref None in
  let wait () =
    Mutex.lock t.mutex;
    match !(t.value) with
    | Some v ->
      t.value := None;
      Waiters.wake_one t.writers () |> ignore;
      Mutex.unlock t.mutex;
      received := Some v
    | None ->
      Waiters.await ~mutex:(Some t.mutex) t.readers t.id
  in
  while !received = None do
    wait ()
  done;
  Option.get !received
