type state =
  | Unlocked                    (* can be locked *)
  | Locked                      (* is locked; threads may be waiting *)
  | Poisoned of exn             (* disabled due to exception in critical section *)

exception Poisoned of exn

type t = {
  id : Ctf.id;
  mutex : Mutex.t;
  mutable state : state;                        (* Owned by [t.mutex] *)
  waiters : [`Take | `Error of exn] Waiters.t;  (* Owned by [t.mutex] *)
}
(* Invariant: t.state <> Locked -> is_empty t.waiters *)

(* When [t.state = Unlocked], [t] owns the user resource that [t] protects.
   [mutex t R] means [t] is a share of a reference to a mutex with an invariant R.
   [locked t] means the holder has the ability to unlock [t]. *)

(* {R} t = create () {mutex t R} *)
let create () =
  let id = Ctf.mint_id () in
  Ctf.note_created id Ctf.Mutex;
  {
    id;
    mutex = Mutex.create ();
    state = Unlocked;                   (* Takes ownership of R *)
    waiters = Waiters.create ();
  }

(* {mutex t R * locked t * R} unlock t {mutex t R}
   If [t] is in an invalid state, it raises an exception and nothing changes. *)
let unlock t =
  Mutex.lock t.mutex;
  (* We now have ownership of [t.state] and [t.waiters]. *)
  Ctf.note_signal t.id;
  match t.state with
  | Unlocked -> 
    Mutex.unlock t.mutex;
    let ex = Sys_error "Eio.Mutex.unlock: already unlocked!" in
    t.state <- Poisoned ex;
    raise ex
  | Locked ->
    begin match Waiters.wake_one t.waiters `Take with
      | `Ok -> ()       (* We transferred [locked t * R] to a waiter; [t] remains [Locked]. *)
      | `Queue_empty -> t.state <- Unlocked     (* The state now owns R. *)
    end;
    Mutex.unlock t.mutex
  | Poisoned ex ->
    Mutex.unlock t.mutex;
    raise (Poisoned ex)

(* {mutex t R} lock t {mutex t R * locked t * R} *)
let lock t =
  Mutex.lock t.mutex;
  match t.state with
  | Locked ->
    Ctf.note_try_read t.id;
    begin match Waiters.await ~mutex:(Some t.mutex) t.waiters t.id with
      | `Error ex -> raise ex   (* Poisoned; stop waiting *)
      | `Take ->
        (* The unlocker didn't change the state, so it's still Locked, as required.
           {locked t * R} *)
        ()
    end
  | Unlocked -> 
    Ctf.note_read t.id;
    t.state <- Locked;          (* We transfer R from the state to our caller. *)
    (* {locked t * R} *)
    Mutex.unlock t.mutex
  | Poisoned ex ->
    Mutex.unlock t.mutex;
    raise (Poisoned ex)

(* {mutex t R} v = try_lock t { mutex t R * if v then locked t * R else [] } *)
let try_lock t =
  Mutex.lock t.mutex;
  match t.state with
  | Locked ->
    Ctf.note_try_read t.id;
    Mutex.unlock t.mutex;
    false
  | Unlocked -> 
    Ctf.note_read t.id;
    t.state <- Locked;          (* We transfer R from the state to our caller. *)
    Mutex.unlock t.mutex;
    (* {locked t * R} *)
    true
  | Poisoned ex ->
    Mutex.unlock t.mutex;
    raise (Poisoned ex)

(* {mutex t R * locked t} poison t ex {mutex t R} *)
let poison t ex =
  Mutex.lock t.mutex;
  t.state <- Poisoned ex;
  Waiters.wake_all t.waiters (`Error (Poisoned ex));
  Mutex.unlock t.mutex

(* {locked t * R} fn () {locked t * R} ->
   {mutex t R} use_ro t fn {mutex t R} *)
let use_ro t fn =
  lock t;
  (* {mutex t R * locked t * R} *)
  match fn () with
  | x -> unlock t; x
  | exception ex -> unlock t; raise ex

(* {locked t * R} v = match fn () with _ -> true | exception _ -> false {locked t * if v then R else []} ->
   {mutex t R} use_rw ~protect t fn {mutex t R} *)
let use_rw ~protect t fn =
  lock t;
  (* {mutex t R * locked t * R} *)
  match if protect then Cancel.protect fn else fn () with
  | x -> unlock t; x
  | exception ex ->
    (* {mutex t R * locked t} *)
    poison t ex;
    raise ex
