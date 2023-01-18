module Locking = struct
  type 'a t = {
    mutex : Mutex.t;

    id : Ctf.id;

    capacity : int;               (* [capacity > 0] *)
    items : 'a Queue.t;

    (* Readers suspended because [items] is empty. *)
    readers : 'a Waiters.t;

    (* Writers suspended because [items] is at capacity. *)
    writers : unit Waiters.t;
  }

  let with_mutex t f =
    Mutex.lock t.mutex;
    match f () with
    | x -> Mutex.unlock t.mutex; x
    | exception ex -> Mutex.unlock t.mutex; raise ex

  (* Invariants *)
  let _validate t =
    with_mutex t @@ fun () ->
    assert (Queue.length t.items <= t.capacity);
    assert (Waiters.is_empty t.readers || Queue.is_empty t.items);
    assert (Waiters.is_empty t.writers || Queue.length t.items = t.capacity)

  let create capacity =
    assert (capacity > 0);
    let id = Ctf.mint_id () in
    Ctf.note_created id Ctf.Stream;
    {
      mutex = Mutex.create ();
      id;
      capacity;
      items = Queue.create ();
      readers = Waiters.create ();
      writers = Waiters.create ();
    }

  let add t item =
    Mutex.lock t.mutex;
    match Waiters.wake_one t.readers item with
    | `Ok -> Mutex.unlock t.mutex
    | `Queue_empty ->
      (* No-one is waiting for an item. Queue it. *)
      if Queue.length t.items < t.capacity then (
        Queue.add item t.items;
        Mutex.unlock t.mutex
      ) else (
        (* The queue is full. Wait for our turn first. *)
        Suspend.enter_unchecked @@ fun ctx enqueue ->
        Waiters.await_internal ~mutex:(Some t.mutex) t.writers t.id ctx (fun r ->
            (* This is called directly from [wake_one] and so we have the lock.
               We're still running in [wake_one]'s domain here. *)
            if Result.is_ok r then (
              (* We get here immediately when called by [take], after removing an item,
                 so there is space *)
              Queue.add item t.items;
            );
            enqueue r
          )
      )

  let take t =
    Mutex.lock t.mutex;
    match Queue.take_opt t.items with
    | None ->
      (* There aren't any items, so we need to wait for one. *)
      Waiters.await ~mutex:(Some t.mutex) t.readers t.id
    | Some v ->
      (* If anyone was waiting for space, let the next one go.
         [is_empty writers || length items = t.capacity - 1] *)
      begin match Waiters.wake_one t.writers () with
        | `Ok                     (* [length items = t.capacity] again *)
        | `Queue_empty -> ()      (* [is_empty writers] *)
      end;
      Mutex.unlock t.mutex;
      v

  let take_nonblocking t =
    Mutex.lock t.mutex;
    match Queue.take_opt t.items with
    | None -> Mutex.unlock t.mutex; None (* There aren't any items. *)
    | Some v ->
      (* If anyone was waiting for space, let the next one go.
         [is_empty writers || length items = t.capacity - 1] *)
      begin match Waiters.wake_one t.writers () with
        | `Ok                     (* [length items = t.capacity] again *)
        | `Queue_empty -> ()      (* [is_empty writers] *)
      end;
      Mutex.unlock t.mutex;
      Some v

  let length t =
    Mutex.lock t.mutex;
    let len = Queue.length t.items in
    Mutex.unlock t.mutex;
    len

  let dump f t =
    Fmt.pf f "<Locking stream: %d/%d items>" (length t) t.capacity
end

type 'a t =
  | Sync of 'a Sync.t
  | Locking of 'a Locking.t

let create = function
  | 0 -> Sync (Sync.create ())
  | capacity -> Locking (Locking.create capacity)

let add t v =
  match t with
  | Sync x -> Sync.put x v
  | Locking x -> Locking.add x v

let take = function
  | Sync x -> Sync.take x
  | Locking x -> Locking.take x

let take_nonblocking = function
  | Sync x -> Sync.take_nonblocking x
  | Locking x -> Locking.take_nonblocking x

let length = function
  | Sync _ -> 0
  | Locking x -> Locking.length x

let is_empty t = (length t = 0)

let dump f = function
  | Sync x -> Sync.dump f x
  | Locking x -> Locking.dump f x
