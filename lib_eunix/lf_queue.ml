(* A lock-free multi-producer, single-consumer, thread-safe queue without support for cancellation.
   This makes a good data structure for a scheduler's run queue.

   See: "Implementing lock-free queues"
   https://people.cs.pitt.edu/~jacklange/teaching/cs2510-f12/papers/implementing_lock_free.pdf
   
   It is simplified slightly because we don't need multiple consumers.
   Therefore [head] is not atomic. *)

type 'a node = {
  next : 'a node Atomic.t;
  mutable value : 'a;
}

type 'a t = {
  tail : 'a node Atomic.t;
  mutable head : 'a node;
}
(* [head] is the last node dequeued (or a dummy node, initially).
   [head.next] gives the real first node, if not [null].
   [tail] is a node that was once at the tail of the queue.
   Follow [tail.next] (if not [null]) to find the real last node. *)

let null : 'a node = Obj.magic ()

let atomic_null () : 'a node Atomic.t = Atomic.make (Obj.magic ())

let push t x =
  let node = { value = x; next = atomic_null () } in
  let rec aux () =
    let p = Atomic.get t.tail in
    (* [p] was the last item in the queue at some point.
       While [p.next == null], it still is. *)
    if Atomic.compare_and_set p.next (Obj.magic null) node then (
      (* [node] has now been added to the queue (and possibly even consumed).
         Update [tail], unless someone else already did it for us. *)
      ignore (Atomic.compare_and_set t.tail p node : bool)
    ) else (
      (* Someone else added a different node first ([p.next] is not [null]).
         Make [t.tail] more up-to-date, if it hasn't already changed, and try again. *)
      ignore (Atomic.compare_and_set t.tail p (Atomic.get p.next) : bool);
      aux ()
    )
  in
  aux ()

let pop t =
  let p = t.head in
  (* [p] is the previously-popped item. *)
  let node = Atomic.get p.next in
  if node == Obj.magic null then None
  else (
    t.head <- node;
    let v = node.value in
    node.value <- Obj.magic ();         (* So it can be GC'd *)
    Some v
  )

let is_empty t =
  Atomic.get t.head.next == Obj.magic null

let create () =
  let dummy = { value = Obj.magic (); next = atomic_null () } in
  { tail = Atomic.make dummy; head = dummy }
