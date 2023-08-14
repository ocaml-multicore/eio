(* A pool is a sequence of cells containing either available slots or consumers waiting for them.
   A slot may or may not contain an actual resource.

   To use a resource:

   1. Get the next "suspend" cell. If it contains a resource slot, use it.
   2. If no slot is ready and we're below capacity, create a new slot and add it (to the next resume cell).
   3. Either way, wait for the cell to be resumed with a slot.
   4. Once you have a slot, ensure it contains a resource, creating one if not.
   5. When done, add the slot back (in the next resume cell).
*)

(* Import these directly because we copy this file for the dscheck tests. *)
module Fiber_context = Eio__core.Private.Fiber_context
module Suspend = Eio__core.Private.Suspend

type 'a slot = 'a option ref

module Cell = struct
  (* The possible behaviours are:

     1. Suspender : In_transition -> Request            Suspender waits for a resource  
        1.1. Resumer : Request -> Finished              Resumer then providers a resource
        1.2. Suspender : Request -> Finished            Suspender cancels
     2. Resumer : In_transition -> Resource             Resumer provides a spare resource
        2.1. Suspender : Resource -> Finished           Suspender doesn't need to wait
  *)

  type 'a t =
    | In_transition
    | Request of ('a slot -> unit)
    | Resource of 'a slot
    | Finished

  let init = In_transition

  let segment_order = 2

  let dump f = function
    | In_transition -> Fmt.string f "In_transition"
    | Request _ -> Fmt.string f "Request"
    | Resource _ -> Fmt.string f "Resource"
    | Finished -> Fmt.string f "Finished"
end

module Q = Cells.Make(Cell)

type 'a t = {
  slots : int Atomic.t;        (* Total resources, available and in use *)
  max_slots : int;
  alloc : unit -> 'a;
  validate : 'a -> bool;
  dispose : 'a -> unit;
  q : 'a Q.t;
}

let create ?(validate=Fun.const true) ?(dispose=ignore) max_size alloc =
  if max_size <= 0 then invalid_arg "Pool.create: max_size is <= 0";
  {
    slots = Atomic.make 0;
    max_slots = max_size;
    alloc;
    validate;
    dispose;
    q = Q.make ();
  }

(* [add t x] adds [x] to the queue of available slots. *)
let rec add t x =
  let cell = Q.next_resume t.q in
  let rec aux () =
    match Atomic.get cell with
    | In_transition -> if not (Atomic.compare_and_set cell In_transition (Resource x)) then aux ()
    | Finished -> add t x         (* The consumer cancelled. Get another cell and retry. *)
    | Request r as prev ->
      if Atomic.compare_and_set cell prev Finished then (
        r x               (* We had a consumer waiting. Give it to them. *)
      ) else add t x      (* Consumer cancelled; retry with another cell. *)
    | Resource _ -> assert false  (* Can't happen; only a resumer can set this, and we're the resumer. *)
  in
  aux ()

(* Try to cancel by transitioning from [Request] to [Finished].
   This can only be called after previously transitioning to [Request]. *)
let cancel segment cell =
  match Atomic.exchange cell Cell.Finished with
  | Request _ -> Q.cancel_cell segment; true
  | Finished -> false                                   (* Already resumed; reject cancellation *)
  | In_transition | Resource _ -> assert false          (* Can't get here from [Request]. *)

(* If [t] is under capacity, add another (empty) slot. *)
let rec maybe_add_slot t =
  let current = Atomic.get t.slots in
  if current < t.max_slots then (
    if Atomic.compare_and_set t.slots current (current + 1) then add t (ref None)
    else maybe_add_slot t       (* Concurrent update; try again *)
  )

(* [run_with t f slot] ensures that [slot] contains a valid resource and then runs [f resource] with it.
   Afterwards, the slot is returned to [t]. *)
let run_with t f slot =
  match
    begin match !slot with
      | Some x when t.validate x -> f x
      | Some x ->
        slot := None;
        t.dispose x;
        let x = t.alloc () in
        slot := Some x;
        f x
      | None ->
        let x = t.alloc () in
        slot := Some x;
        f x
    end
  with
  | r -> 
    add t slot;
    r
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    add t slot;
    Printexc.raise_with_backtrace ex bt

let use t f =
  let segment, cell = Q.next_suspend t.q in
  match Atomic.get cell with
  | Finished | Request _ -> assert false
  | Resource slot ->
    Atomic.set cell Finished;   (* Allow value to be GC'd *)
    run_with t f slot
  | In_transition ->
      (* It would have been better if more resources were available.
         If we still have capacity, add a new slot now. *)
      maybe_add_slot t;
      (* No item is available right now. Start waiting *)
      let slot =
        Suspend.enter_unchecked (fun ctx enqueue ->
            let r x = enqueue (Ok x) in
            if Atomic.compare_and_set cell In_transition (Request r) then (
              match Fiber_context.get_error ctx with
              | Some ex ->
                if cancel segment cell then enqueue (Error ex);
                (* else being resumed *)
              | None ->
                Fiber_context.set_cancel_fn ctx (fun ex ->
                    if cancel segment cell then enqueue (Error ex)
                    (* else being resumed *)
                  )
            ) else (
              match Atomic.exchange cell Finished with
              | Resource x -> enqueue (Ok x)
              | _ -> assert false
            );
          )
      in
      (* assert (Atomic.get cell = Finished); *)
      run_with t f slot
