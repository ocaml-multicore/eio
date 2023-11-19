(* A lock-free multi-producer, single-consumer, thread-safe queue without support for cancellation.
   This makes a good data structure for a scheduler's run queue.

   Based on Vesa Karvonen's examaple at:
   https://github.com/ocaml-multicore/picos/blob/07d6c2d391e076b490098c0379d01208b3a9cc96/test/lib/foundation/mpsc_queue.ml
*)

exception Closed

(* A list where the end indicates whether the queue is closed. *)
type 'a clist =
  | (::) of 'a * 'a clist
  | Open
  | Closed

(* [rev_append l1 l2] is like [rev l1 @ l2] *)
let rec rev_append l1 l2 =
  match l1 with
  | a :: l -> rev_append l (a :: l2)
  | Open -> l2
  | Closed -> assert false
    
let[@tail_mod_cons] rec ( @ ) l1 l2 =
  match l1 with
  | h1 :: tl -> h1 :: (tl @ l2)
  | Open -> l2
  | Closed -> assert false

(* The queue contains [head @ rev tail].
   If [tail] is non-empty then it ends in [Open]. *)
type 'a t = {
  mutable head : 'a clist;
  tail : 'a clist Atomic.t;
}

let rec push t x =
  match Atomic.get t.tail with
  | Closed -> raise Closed
  | before ->
    let after = x :: before in
    if not (Atomic.compare_and_set t.tail before after) then
      push t x

let push_head t x =
  match t.head with
  | Closed -> raise Closed
  | before -> t.head <- x :: before

let rec pop t =
  match t.head with
  | x :: xs -> t.head <- xs; Some x
  | Closed -> raise Closed
  | Open ->
    (* We know the tail is open because we just saw the head was open
       and we don't run concurrently with [close]. *)
    match Atomic.exchange t.tail Open with
    | Closed -> assert false
    | Open -> None              (* Optimise the common case *)
    | tail ->
      t.head <- rev_append tail Open;
      pop t

let close t =
  match Atomic.exchange t.tail Closed with
  | Closed -> invalid_arg "Lf_queue already closed!"
  | xs -> t.head <- t.head @ rev_append xs Closed

let is_empty t =
  match t.head with
  | _ :: _ -> false
  | Closed -> raise Closed
  | Open ->
    match Atomic.get t.tail with
    | _ :: _ -> false
    | _ -> true

let create () = {
  head = Open;
  tail = Atomic.make Open;
}
