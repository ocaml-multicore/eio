(* A queue built on cells.ml using the "simple" cancellation mode,
   where resuming a cancelled request does nothing instead of retrying. *)

module Make(Config : sig val segment_order : int end) = struct
  module Cell = struct
    type _ t =
      | Empty
      | Value of int
      | Waiting of (int -> unit)
      | Cancelled
      | Finished

    let init = Empty

    let segment_order = Config.segment_order

    let dump f = function
      | Empty -> Fmt.string f "Empty"
      | Value v -> Fmt.pf f "Value %d" v
      | Waiting _  -> Fmt.string f "Waiting"
      | Cancelled -> Fmt.string f "Cancelled"
      | Finished -> Fmt.string f "Finished"
  end

  module Cells = Cells.Make(Cell)

  let cancel (segment, cell) =
    match Atomic.get cell with
    | Cell.Waiting _ as prev ->
      if Atomic.compare_and_set cell prev Cancelled then (
        Cells.cancel_cell segment;
        true
      ) else (
        false
      )
    | Finished -> false
    | _ -> assert false

  let resume t v =
    let cell = Cells.next_resume t in
    if not (Atomic.compare_and_set cell Cell.Empty (Value v)) then (
      match Atomic.get cell with
      | Waiting w as prev ->
        if Atomic.compare_and_set cell prev Finished then w v
      (* else cancelled *)
      | Cancelled -> ()
      | Empty | Value _ | Finished -> assert false
    )

  let suspend t k =
    let segment, cell = Cells.next_suspend t in
    if Atomic.compare_and_set cell Cell.Empty (Waiting k) then Some (segment, cell)
    else (
      match Atomic.get cell with
      | Value v -> Atomic.set cell Finished; k v; None
      | Cancelled | Empty | Waiting _ | Finished -> assert false
    )

  let make = Cells.make

  let dump = Cells.dump
end
