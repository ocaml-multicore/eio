(* Note on thread-safety

   Promises can be shared between domains, so everything here must be thread-safe.

   Wrapping everything in a mutex would be one way to do that, but that makes reads
   slow, and only one domain would be able to read at a time.

   Instead, we use an Atomic to hold the state, plus an additional mutex for the waiters
   while in the Unresolved state. This makes resolved promises faster (at the cost of
   making operations on unresolved promises a bit slower). It also makes reasoning about
   the code more fun.

   We can think of atomics and mutexes as "boxes", containing values and
   invariants. To use them, you open the box to get access to the contents,
   then close the box afterwards, restoring the invariant. For mutexes,
   open/close is lock/unlock. For atomics, every operation implicitly opens and
   closes the box. Any number of callers can share a reference to the box
   itself; the runtime ensures a box can only be opened by one user at a time.

   We can hold a full reference to something (meaning no-one else has access to it
   and we can mutate it), or a fraction (giving us read-only access but also
   ensuring that no-one else can mutate it either). *)

type 'a state =
  | Fulfilled of 'a
  | Broken of exn
  | Unresolved of ('a, exn) result Waiters.t * Mutex.t
  (* The Unresolved state's mutex box contains:
     - Full access to the Waiters.
     - Half access to the promise's state.
     - The invariant that if the promise is resolved then the waiters list is empty. *)

type !'a t = {
  id : Ctf.id;

  state : 'a state Atomic.t;
  (* This atomic box contains either:
     - A non-zero share of the reference to the Fulfilled or Broken state.
     - A half-share of the reference to the Unresolved state. *)
}

type 'a u = 'a t

let create_with_id id =
  let t = {
    id;
    state = Atomic.make (Unresolved (Waiters.create (), Mutex.create ()));
  } in
  t, t

let create ?label () =
  let id = Ctf.mint_id () in
  Ctf.note_created ?label id Ctf.Promise;
  create_with_id id

let fulfilled x =
  let id = Ctf.mint_id () in
  Ctf.note_created id Ctf.Promise;
  { id; state = Atomic.make (Fulfilled x) }

let broken ex =
  let id = Ctf.mint_id () in
  Ctf.note_created id Ctf.Promise;
  { id; state = Atomic.make (Broken ex) }

let await_result t =
  match Atomic.get t.state with
  (* If the atomic is resolved, we take a share of that reference and return
     the remainder to the atomic (which will still be non-zero). We can then
     continue to know that the promise is resolved after the [Atomic.get]. *)
  | Fulfilled x ->
    Ctf.note_read t.id;
    Ok x
  | Broken ex ->
    Ctf.note_read t.id;
    Error ex
  | Unresolved (q, mutex) ->
    (* We discovered that the promise was unresolved, but we can't be sure it still is,
       since we had to return the half-share reference to the atomic. So the [get] is
       just to get access to the mutex. *)
    Ctf.note_try_read t.id;
    Mutex.lock mutex;
    (* Having opened the mutex, we have:
       - Access to the waiters.
       - Half access to the promise's state (so we know it can't change until we close the mutex).
       - The mutex invariant. *)
    match Atomic.get t.state with
    | Unresolved _ ->
      (* The promise is unresolved, and can't change while we hold the mutex.
         It's therefore safe to add a new waiter (and let [Waiters.await] close the mutex). *)
      Waiters.await ~mutex:(Some mutex) q t.id
    (* Otherwise, the promise was resolved by the time we took the lock.
       Release the lock (which is fine, as we didn't change anything). *)
    | Fulfilled x ->
      Mutex.unlock mutex;
      Ctf.note_read t.id;
      Ok x
    | Broken ex ->
      Mutex.unlock mutex;
      Ctf.note_read t.id;
      Error ex

let await t =
  match await_result t with
  | Ok x -> x
  | Error ex -> raise ex

let rec fulfill t v =
  match Atomic.get t.state with
  | Broken ex -> invalid_arg ("Can't fulfill already-broken promise: " ^ Printexc.to_string ex)
  | Fulfilled _ -> invalid_arg "Can't fulfill already-fulfilled promise"
  | Unresolved (q, mutex) as prev ->
    (* The above [get] just gets us access to the mutex;
       By the time we get here, the promise may have become resolved. *)
    Mutex.lock mutex;
    (* Having opened the mutex, we have:
       - Access to the waiters.
       - Half access to the promise's state (so we know it can't change until we close the mutex).
       - The mutex invariant.
       Now we open the atomic again, getting the other half access. Together,
       this gives us full access to the state (i.e. no-one else can be using
       it), allowing us to change it.
       Note: we don't actually need an atomic CAS here, just a get and a set
       would do, but this seems simplest. *)
    if Atomic.compare_and_set t.state prev (Fulfilled v) then (
      (* The atomic now has half-access to the fullfilled state (which counts
         as non-zero), and we have the other half. Now we need to restore the
         mutex invariant by clearing the wakers. *)
      Ctf.note_resolved t.id ~ex:None;
      Waiters.wake_all q (Ok v);
      Mutex.unlock mutex
    ) else (
      (* Otherwise, the promise was already fulfilled when we opened the mutex.
         Close it without any changes and retry. *)
      Mutex.unlock mutex;
      fulfill t v
    )

let rec break t ex =
  match Atomic.get t.state with
  | Broken orig -> invalid_arg (Printf.sprintf "Can't break already-broken promise: %s -> %s"
                                  (Printexc.to_string orig) (Printexc.to_string ex))
  | Fulfilled _ -> invalid_arg (Printf.sprintf "Can't break already-fulfilled promise (with %s)"
                                  (Printexc.to_string ex))
  | Unresolved (q, mutex) as prev ->
    (* Same logic as for [fulfill]. *)
    Mutex.lock mutex;
    if Atomic.compare_and_set t.state prev (Broken ex) then (
      Ctf.note_resolved t.id ~ex:(Some ex);
      Waiters.wake_all q (Error ex);
      Mutex.unlock mutex
    ) else (
      Mutex.unlock mutex;
      break t ex
    )

let resolve t = function
  | Ok x -> fulfill t x
  | Error ex -> break t ex

let state t =
  match Atomic.get t.state with
  | Unresolved _ -> `Unresolved
  | Fulfilled x -> `Fulfilled x
  | Broken ex -> `Broken ex

let id t = t.id

let is_resolved t =
  match Atomic.get t.state with
  | Fulfilled _ | Broken _ -> true
  | Unresolved _ -> false
