(* A lock-free multi-producer, single-consumer, thread-safe queue without support for cancellation.
   This makes a good data structure for a scheduler's run queue.

   See: "Implementing lock-free queues"
   https://people.cs.pitt.edu/~jacklange/teaching/cs2510-f12/papers/implementing_lock_free.pdf
   
   It is simplified slightly because we don't need multiple consumers.
   Therefore [head] is not atomic. *)

exception Closed

module Node : sig
  type 'a t = {
    next : 'a opt Atomic.t;
    mutable value : 'a;
  }
  and +'a opt

  val make : 'a -> 'a t

  val none : 'a opt
  (** [t.next = none] means that [t] is currently the last node. *)

  val closed : 'a opt
  (** [t.next = closed] means that [t] will always be the last node. *)

  val some : 'a t -> 'a opt
  val fold : 'a opt -> none:(unit -> 'b) -> some:('a t -> 'b) -> 'b
end = struct
  (* https://github.com/ocaml/RFCs/pull/14 should remove the need for magic here *)

  type +'a opt  (* special | 'a t *)

  type 'a t = {
    next : 'a opt Atomic.t;
    mutable value : 'a;
  }

  type special =
    | Nothing
    | Closed

  let none : 'a. 'a opt = Obj.magic Nothing
  let closed : 'a. 'a opt = Obj.magic Closed
  let some (t : 'a t) : 'a opt = Obj.magic t

  let fold (opt : 'a opt) ~none:n ~some =
    if opt == none then n ()
    else if opt == closed then raise Closed
    else some (Obj.magic opt : 'a t)

  let make value = { value; next = Atomic.make none }
end

type 'a t = {
  tail : 'a Node.t Atomic.t;
  mutable head : 'a Node.t;
}
(* [head] is the last node dequeued (or a dummy node, initially).
   [head.next] gives the real first node, if not [Node.none].
   [tail] is a node that was once at the tail of the queue.
   Follow [tail.next] (if not [none]) to find the real last node. *)

let push t x =
  let node = Node.make x in
  let rec aux () =
    let p = Atomic.get t.tail in
    (* [p] was the last item in the queue at some point.
       While [p.next == none], it still is. *)
    if Atomic.compare_and_set p.next Node.none (Node.some node) then (
      (* [node] has now been added to the queue (and possibly even consumed).
         Update [tail], unless someone else already did it for us. *)
      ignore (Atomic.compare_and_set t.tail p node : bool)
    ) else (
      (* Someone else added a different node first ([p.next] is not [none]).
         Make [t.tail] more up-to-date, if it hasn't already changed, and try again. *)
      Node.fold (Atomic.get p.next)
        ~none:(fun () -> assert false)
        ~some:(fun p_next ->
            ignore (Atomic.compare_and_set t.tail p p_next : bool);
            aux ()
          )
    )
  in
  aux ()

let rec close (t:'a t) =
  (* Mark the tail node as final. *)
  let p = Atomic.get t.tail in
  if not (Atomic.compare_and_set p.next Node.none Node.closed) then (
    (* CAS failed because [p] is no longer the tail (or is already closed). *)
    Node.fold (Atomic.get p.next)
      ~none:(fun () -> assert false)    (* Can't switch from another state to [none] *)
      ~some:(fun p_next ->
          (* Make [tail] more up-to-date if it hasn't changed already *)
          ignore (Atomic.compare_and_set t.tail p p_next : bool);
          (* Retry *)
          close t
        )
  )

let pop t =
  let p = t.head in
  (* [p] is the previously-popped item. *)
  let node = Atomic.get p.next in
  Node.fold node
    ~none:(fun () -> None)
    ~some:(fun node ->
        t.head <- node;
        let v = node.value in
        node.value <- Obj.magic ();         (* So it can be GC'd *)
        Some v
      )

let is_empty t =
  Node.fold (Atomic.get t.head.next)
    ~none:(fun () -> true)
    ~some:(fun _ -> false)

let create () =
  let dummy = { Node.value = Obj.magic (); next = Atomic.make Node.none } in
  { tail = Atomic.make dummy; head = dummy }
