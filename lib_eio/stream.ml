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

  let select_of_many streams_fns =
    let finished = Atomic.make false in
    let cancel_fns = ref [] in
    let add_cancel_fn fn = cancel_fns := fn :: !cancel_fns in
    let cancel_all () = List.iter (fun fn -> fn ()) !cancel_fns in
    let wait ctx enqueue (t, f) = begin
      Mutex.lock t.mutex;
      (* First check if any items are already available and return early if there are. *)
      if not (Queue.is_empty t.items)
      then (
        cancel_all ();
        Mutex.unlock t.mutex;
        enqueue (Ok (f (Queue.take t.items))))
      else add_cancel_fn @@
        (* Otherwise, register interest in this stream. *)
        Waiters.cancellable_await_internal ~mutex:(Some t.mutex) t.readers t.id ctx (fun r ->
            if Result.is_ok r then (
              if not (Atomic.compare_and_set finished false true) then (
                (* Another stream has yielded an item in the meantime. However, as
                   we have been waiting on this stream it must have been empty.

                   As the stream's mutex was held since before last checking for an item,
                   the queue must be empty.
                *)
                assert ((Queue.length t.items) < t.capacity);
                Queue.add (Result.get_ok r) t.items
              ) else (
                (* remove all other entries of this fiber in other streams' waiters. *)
                cancel_all ()
              ));
            (* item is returned to waiting caller through enqueue and enter_unchecked. *)
            enqueue (Result.map f r))
    end in
    (* Register interest in all streams and return first available item. *)
    let wait_for_stream streams_fns = begin
      Suspend.enter_unchecked (fun ctx enqueue -> List.iter (wait ctx enqueue) streams_fns)
    end in
    wait_for_stream streams_fns

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

let select streams =
  let filter s = match s with
    | (Sync _, _) -> assert false
    | (Locking x, f) -> (x, f)
  in
  Locking.select_of_many (List.map filter streams)

let length = function
  | Sync _ -> 0
  | Locking x -> Locking.length x

let is_empty t = (length t = 0)

let dump f = function
  | Sync x -> Sync.dump f x
  | Locking x -> Locking.dump f x
