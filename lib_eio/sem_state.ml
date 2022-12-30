(* A lock-free semaphore, using Cells.

   We have a number of resources (1 in the case of a mutex). Each time a user
   wants a resource, the user decrements the counter. When finished with the
   resource, the user increments the counter again.

   If there are more users than resources then the counter will be negative. If
   a user decrements the counter to a non-negative value then it gets ownership
   of one of the free resources. If it decrements the counter to a negative
   value then it must wait (by allocating a cell). When a user with a resource
   increments the counter *from* a negative value, that user is responsible for
   resuming one waiting cell (transferring ownership of the resource). This
   ensures that every waiter will get woken exactly once.

   Cancellation

   We could consider cancelling a request to be simply replacing the callback
   with a dummy one that immediately releases the resource. However, if callers
   keep cancelling then the list of cancelled requests would keep growing.

   Instead, we'd like cancellation simply to undo the effects of suspending, by
   incrementing the counter and marking the cell as Finished (so that the
   resumer will ignore it and move on to the next waiter, and the Finished
   cell can be freed).

   If the cancelling user increments from a negative value then it is responsible
   for waking one user, which is fine as it is waking itself. However, it may find
   itself incrementing from a non-negative one if it is racing with a resumer
   (if the count is non-negative then once all current operations finish there
   would be no suspended users, so the process of waking this user must have
   already begun).

   To handle this, a cancelling user first transitions the cell to In_transition,
   then increments the counter, then transitions to the final Finished state,
   in the usual case where it incremented from a negative value.

   If a resumer runs at the same time then it may also increment the counter
   from a non-negative value and try to wake this cell. It transitions the cell
   from In_transition to Finished. The cancelling user will notice this when it
   fails to CAS to Finished and can handle it.

   If the cancelling user sees the Finished state after In_transition then it
   knows that the resuming user has transferred to it the responsibility of
   waking one user. If the cancelling user is also responsible for waking one
   user then it performs an extra resume on behalf of the resuming user.

   Finally, if the cancelling user is not responsible for waking anyone (even
   itself) then it leaves the cell in In_transition (the CQS paper uses a
   separate Refused state, but we don't actually need that). This can only
   happen when a resume is happening at the same time. The resumer will
   transition to Finished, creating an obligation to resume, but we've just
   done that anyway. We know this In_transition state can't last long because
   at the moment when the canceller incremented the counter all current
   waiters, including itself, were in the process of being resumed. *)

module Cell = struct
  type _ t =
    | In_transition                     (* The suspender will try to CAS this soon. *)
    | Request of (unit -> unit)         (* Waiting for a resource. *)
    | Finished                          (* Ownership of the resource has been transferred,
                                           or the suspender cancelled. *)

  let init = In_transition
  (* We only resume when we know another thread is suspended or in the process of suspending. *)

  let segment_order = 2

  let dump f = function
    | Request _ -> Fmt.string f "Request"
    | Finished -> Fmt.string f "Finished"
    | In_transition -> Fmt.string f "In_transition"
end

module Cells = Cells.Make(Cell)

type cell = unit Cell.t

type t = {
  state : int Atomic.t;         (* Free resources. Negative if there are waiters waiting. *)
  cells : unit Cells.t;
}

type request = t * unit Cells.segment * unit Cell.t Atomic.t

(* Wake one waiter (and give it the resource being released). *)
let rec resume t =
  let cell = Cells.next_resume t.cells in
  match (Atomic.exchange cell Finished : cell) with
  | Request r ->
    (* The common case: there was a waiter for the value.
       We pass ownership of the resource to it. *)
    r ()
  | Finished ->
    (* The waiter has finished cancelling. Ignore it and resume the next one. *)
    resume t
  | In_transition ->
    (* The consumer is in the middle of doing something and will soon try to
       CAS to a new state. It will see that we got there first and handle the
       resume when it's done. *)
    ()

(* [true] on success, or [false] if we need to suspend.
   You MUST call [suspend] iff this returns [false].
   The reason for splitting this is because e.g. [Semaphore] needs to get
   the continuation for the fiber between [acquire] and [suspend]. *)
let acquire t =
  let s = Atomic.fetch_and_add t.state (-1) in
  (* We got a resource if we decremented *to* a non-negative number,
     which happens if we decremented *from* a positive one. *)
  s > 0

let suspend t k : request option =
  let (segment, cell) = Cells.next_suspend t.cells in
  if Atomic.compare_and_set cell In_transition (Request k) then Some (t, segment, cell)
  else match Atomic.get cell with
    | Finished ->
      (* We got resumed before we could add the waiter. *)
      k ();
      None
    | Request _ | In_transition ->
      (* These are unreachable from the previously-observed non-In_transition state
         without us taking some action first *)
      assert false

let release t =
  let s = Atomic.fetch_and_add t.state (+1) in
  if s < 0 then (
    (* We incremented from a negative value.
       We are therefore responsible for waking one waiter. *)
    resume t
  )

let cancel (t, segment, cell) =
  match (Atomic.get cell : cell) with
  | Request _ as old ->
    if Atomic.compare_and_set cell old In_transition then (
      (* Undo the effect of [acquire] by incrementing the counter.
         As always, if we increment from a negative value then we need to resume one waiter. *)
      let need_resume = Atomic.fetch_and_add t.state (+1) < 0 in
      if need_resume then (
        if Atomic.compare_and_set cell In_transition Finished then (
          (* The normal case. We resumed ourself by cancelling.
             This is the only case we need to tell the segment because in all
             other cases the resumer has already reached this segment so
             freeing it is pointless. *)
          Cells.cancel_cell segment
        ) else (
          (* [release] got called at the same time and it also needed to resume one waiter.
             So we call [resume] to handle the extra one, in addition to resuming ourself. *)
          resume t
        )
      ) else (
        (* This can only happen if [release] ran at the same time and incremented the counter
           before we did. Since we were suspended, and later we saw the counter
           show that no one was, it must have decided to wake us. Either it has placed Finished
           in the cell, or it's about to do so. Either way, we discharge the obligation to
           wake someone by resuming ourself with a cancellation.
           The resource returns to the free pool. We know the resumer has already finished with it
           even if it hasn't updated the cell state yet. *)
      );
      true
    ) else false          (* We got resumed first *)
  | Finished -> false     (* We got resumed first *)
  | In_transition -> invalid_arg "Already cancelling!"

let dump f t =
  Fmt.pf f "Semaphore (state=%d)@,%a"
    (Atomic.get t.state)
    Cells.dump t.cells

let create n =
  if n < 0 then raise (Invalid_argument "n < 0");
  {
    cells = Cells.make ();
    state = Atomic.make n;
  }
