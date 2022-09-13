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
  | Resolved of 'a
  | Unresolved of 'a Waiters.t * Mutex.t
  (* The Unresolved state's mutex box contains:
     - Full access to the Waiters.
     - Half access to the promise's state.
     - The invariant that if the promise is resolved then the waiters list is empty. *)

type !'a promise = {
  id : Ctf.id;

  state : 'a state Atomic.t;
  (* This atomic box contains either:
     - A non-zero share of the reference to the Resolved state.
     - A half-share of the reference to the Unresolved state. *)
}

type +!'a t
type -!'a u

type 'a or_exn = ('a, exn) result t

let to_public_promise : 'a promise -> 'a t = Obj.magic
let to_public_resolver : 'a promise -> 'a u = Obj.magic
let of_public_promise : 'a t -> 'a promise = Obj.magic
let of_public_resolver : 'a u -> 'a promise = Obj.magic

let create_with_id id =
  let t = {
    id;
    state = Atomic.make (Unresolved (Waiters.create (), Mutex.create ()));
  } in
  to_public_promise t, to_public_resolver t

let create ?label () =
  let id = Ctf.mint_id () in
  Ctf.note_created ?label id Ctf.Promise;
  create_with_id id

let create_resolved x =
  let id = Ctf.mint_id () in
  Ctf.note_created id Ctf.Promise;
  to_public_promise { id; state = Atomic.make (Resolved x) }

let await t =
  let t = of_public_promise t in
  match Atomic.get t.state with
  (* If the atomic is resolved, we take a share of that reference and return
     the remainder to the atomic (which will still be non-zero). We can then
     continue to know that the promise is resolved after the [Atomic.get]. *)
  | Resolved x ->
    Ctf.note_read t.id;
    x
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
    | Resolved x ->
      Mutex.unlock mutex;
      Ctf.note_read t.id;
      x

let await_exn t =
  match await t with
  | Ok x -> x
  | Error ex -> raise ex

let resolve t v =
  let rec resolve' t v =
    match Atomic.get t.state with
    | Resolved _ -> invalid_arg "Can't resolve already-resolved promise"
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
      if Atomic.compare_and_set t.state prev (Resolved v) then (
        (* The atomic now has half-access to the fullfilled state (which counts
           as non-zero), and we have the other half. Now we need to restore the
           mutex invariant by clearing the wakers. *)
        Ctf.note_resolved t.id ~ex:None;
        Waiters.wake_all q v;
        Mutex.unlock mutex
      ) else (
        (* Otherwise, the promise was already resolved when we opened the mutex.
           Close it without any changes and retry. *)
        Mutex.unlock mutex;
        resolve' t v
      )
  in
  resolve' (of_public_resolver t) v

let resolve_ok    u x = resolve u (Ok x)
let resolve_error u x = resolve u (Error x)

let peek t =
  let t = of_public_promise t in
  match Atomic.get t.state with
  | Unresolved _ -> None
  | Resolved x -> Some x

let id t =
  let t = of_public_promise t in
  t.id

let is_resolved t =
  Option.is_some (peek t)
