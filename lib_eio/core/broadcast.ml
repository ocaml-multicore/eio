(* See the Cells module for an overview of this system.

   Each new waiter atomically increments the "suspend" pointer and writes
   a callback there. The waking fiber removes all the callbacks and calls them.
   In this version, "resume" never gets ahead of "suspend" (broadcasting just
   brings it up-to-date with the "suspend" pointer).

   When the resume fiber runs, some of the cells reserved for callbacks might
   not yet have been filled. In this case, the resuming fiber just marks them
   as needing to be resumed. When the suspending fiber continues, it will
   notice this and continue immediately. *)

module Cell = struct
  (* For any given cell, there are two actors running in parallel: the
     suspender and the resumer.

     The resumer only performs a single operation (resume).

     The consumer waits to be resumed and then, optionally, cancels.

     This means we only have three cases to think about:

     1. Consumer adds request (Empty -> Request).
        1a. Provider fulfills it (Request -> Resumed).
        1b. Consumer cancels it (Request -> Cancelled).
     2. Provider gets to cell first (Empty -> Resumed).
        When the consumer tries to wait, it resumes immediately.

     The Resumed state should never been seen. It exists only to allow the
     request to be GC'd promptly. We could replace it with Empty, but having
     separate states is clearer for debugging. *)

  type _ t =
    | Request of (unit -> unit)
    | Cancelled
    | Resumed
    | Empty

  let init = Empty

  let segment_order = 2

  let dump f = function
    | Request _ -> Fmt.string f "Request"
    | Empty -> Fmt.string f "Empty"
    | Resumed -> Fmt.string f "Resumed"
    | Cancelled -> Fmt.string f "Cancelled"
end

module Cells = Cells.Make(Cell)

type cell = unit Cell.t
type t = unit Cells.t

type request = unit Cells.segment * cell Atomic.t

let rec resume cell =
  match (Atomic.get cell : cell) with
  | Request r as cur ->
    (* The common case: we have a waiter for the value *)
    if Atomic.compare_and_set cell cur Resumed then r ();
    (* else it was cancelled at the same time; ignore *)
  | Empty ->
    (* The consumer has reserved this cell but not yet stored the request.
       We place Resumed there and it will handle it soon. *)
    if Atomic.compare_and_set cell Empty Resumed then
      ()                (* The consumer will deal with it *)
    else
      resume cell       (* The Request was added concurrently; use it *)
  | Cancelled -> ()
  | Resumed ->
    (* This state is unreachable because we (the provider) haven't set this yet *)
    assert false

let cancel (segment, cell) =
  match (Atomic.get cell : cell) with
  | Request _ as old ->
    if Atomic.compare_and_set cell old Cancelled then (
      Cells.cancel_cell segment;
      true
    ) else false          (* We got resumed first *)
  | Resumed -> false      (* We got resumed first *)
  | Cancelled -> invalid_arg "Already cancelled!"
  | Empty ->
    (* To call [cancel] the user needs a [request] value,
       which they only get once we've reached the [Request] state.
       [Empty] is unreachable from [Request]. *)
    assert false

let suspend t k =
  let (_, cell) as request = Cells.next_suspend t in
  if Atomic.compare_and_set cell Empty (Request k) then Some request
  else match Atomic.get cell with
    | Resumed ->
      (* Resumed before we could add the waiter *)
      k ();
      None
    | Cancelled | Request _ | Empty ->
      (* These are unreachable from the previously-observed non-Empty state
         without us taking some action first *)
      assert false

let resume_all t =
  Cells.resume_all t resume

let create = Cells.make

let dump f t = Cells.dump f t
