type 'a t = {
  id : Ctf.id;

  capacity : int;
  items : 'a Queue.t;

  (* Readers suspended because [items] is empty. *)
  readers : 'a Waiters.t;

  (* Writers suspended because [items] is at capacity. *)
  writers : unit Waiters.t;
}

(* Invariants *)
let validate t =
  assert (Queue.length t.items <= t.capacity);
  assert (Waiters.is_empty t.readers || Queue.is_empty t.items);
  assert (Waiters.is_empty t.writers || Queue.length t.items = t.capacity)

let create capacity =
  assert (capacity >= 0);
  let id = Ctf.mint_id () in
  Ctf.note_created id Ctf.Stream;
  {
    id;
    capacity;
    items = Queue.create ();
    readers = Waiters.create ();
    writers = Waiters.create ();
  }

let add ?sw t item =
  Option.iter Switch.check sw;
  match Waiters.wake_one t.readers (Ok item) with
  | `Ok -> ()
  | `Queue_empty ->
    (* No-one is waiting for an item. Queue it. *)
    if Queue.length t.items < t.capacity then Queue.add item t.items
    else (
      (* The queue is full. Wait for our turn first. *)
      Suspend.enter @@ fun tid enqueue ->
      Switch.await_internal ?sw t.writers t.id tid (fun r ->
          if Result.is_ok r then (
            (* We get here immediately when called by [take], either:
               1. after removing an item, so there is space, or
               2. if [capacity = 0]; [take] will immediately remove the new item. *)
            Queue.add item t.items;
          );
          enqueue r
        )
    )

let take ?sw t =
  Option.iter Switch.check sw;
  match Queue.take_opt t.items with
  | None ->
    (* There aren't any items, so we probably need to wait for one.
       However, there's also the special case of a zero-capacity queue to deal with.
       [is_empty writers || capacity = 0] *)
    begin match Waiters.wake_one t.writers (Ok ()) with
      | `Queue_empty -> Switch.await ?sw t.readers t.id
      | `Ok ->
        (* [capacity = 0] (this is the only way we can get waiters and no items).
           [wake_one] has just added an item to the queue, so remove it quickly to restore the invariant. *)
        Queue.take t.items
    end
  | Some v ->
    (* If anyone was waiting for space, let the next one go.
       [is_empty writers || length items = t.capacity - 1] *)
    begin match Waiters.wake_one t.writers (Ok ()) with
      | `Ok                     (* [length items = t.capacity] again *)
      | `Queue_empty -> ()      (* [is_empty writers] *)
    end;
    v
