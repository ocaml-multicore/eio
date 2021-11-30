(* A lock-free multi-producer, single-consumer, thread-safe queue without support for cancellation.
   This makes a good data structure for a scheduler's run queue.

   See: "Implementing lock-free queues"
   https://people.cs.pitt.edu/~jacklange/teaching/cs2510-f12/papers/implementing_lock_free.pdf
   
   It is simplified slightly because we don't need multiple consumers.
   Therefore [head] is not atomic. *)

type 'a node = {
  next : 'a node option Atomic.t;
  mutable value : 'a;
}

type 'a t = {
  tail : 'a node Atomic.t;
  mutable head : 'a node;
}
(* [head] is the last node dequeued (or a dummy node, initially).
   [head.next] gives the real first node, if not [None].
   [tail] is a node that was once at the tail of the queue.
   Follow [tail.next] (if not [None]) to find the real last node. *)

let push t x =
  let node = { value = x; next = Atomic.make None } in
  let rec aux () =
    let p = Atomic.get t.tail in
    (* [p] was the last item in the queue at some point.
       While [p.next = None], it still is. *)
    if Atomic.compare_and_set p.next None (Some node) then (
      (* [node] has now been added to the queue (and possibly even consumed).
         Update [tail], unless someone else already did it for us. *)
      ignore (Atomic.compare_and_set t.tail p node : bool)
    ) else (
      (* Someone else added a different node first ([p.next] is not [None]).
         Make [t.tail] more up-to-date, if it hasn't already changed, and try again. *)
      ignore (Atomic.compare_and_set t.tail p (Option.get (Atomic.get p.next)) : bool);
      aux ()
    )
  in
  aux ()

let pop t =
  let p = t.head in
  (* [p] is the previously-popped item. *)
  match Atomic.get p.next with
  | Some node ->
    t.head <- node;
    let v = node.value in
    node.value <- Obj.magic ();         (* So it can be GC'd *)
    Some v
  | None -> None

let is_empty t =
  Atomic.get t.head.next = None

let create () =
  let dummy = { value = Obj.magic (); next = Atomic.make None } in
  { tail = Atomic.make dummy; head = dummy }
