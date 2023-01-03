module type CELL = sig
  type 'a t
  val init : 'a t
  val segment_order : int
  val dump : _ t Fmt.t
end

(* To avoid worrying about wrapping on 32-bit platforms,
   we use 63-bit integers for indexes in all cases.
   On 64-bit platforms, this is just [int]. *)
module Int63 = struct
  include Optint.Int63

  (* Fallback for 32-bit platforms. *)
  let rec fetch_and_add_fallback t delta =
    let old = Atomic.get t in
    if Atomic.compare_and_set t old (add old (of_int delta)) then old
    else fetch_and_add_fallback t delta

  let fetch_and_add : t Atomic.t -> int -> t =
    match is_immediate with
    | True -> Atomic.fetch_and_add
    | False -> fetch_and_add_fallback
end

module Make(Cell : CELL) = struct
  let cells_per_segment = 1 lsl Cell.segment_order
  let segment_mask = cells_per_segment - 1

  (* An index identifies a cell. It is a pair of the segment ID and the offset
     within the segment, packed into a single integer so we can increment it
     atomically. *)
  module Index : sig
    type t
    type segment_id = Int63.t

    val of_segment : segment_id -> t
    (* [of_segment x] is the index of the first cell in segment [x]. *)

    val segment : t -> segment_id
    val offset : t -> int

    val zero : t
    val succ : t -> t
    val pred : t -> t

    val next : t Atomic.t -> t

    (* val pp : t Fmt.t *)
  end = struct
    type t = Int63.t
    type segment_id = Int63.t

    let segment t = Int63.shift_right_logical t Cell.segment_order
    let of_segment id = Int63.shift_left id Cell.segment_order

    let offset t = Int63.to_int t land segment_mask

    let zero = Int63.zero
    let succ = Int63.succ
    let pred = Int63.pred

    let next t_atomic =
      Int63.fetch_and_add t_atomic (+1)

    (* let pp f t = Fmt.pf f "%d:%d" (segment t) (offset t) *)
  end

  (* A pair with counts for the number of cancelled cells in a segment and the
     number of pointers to it, packed as an integer so it can be adjusted atomically. *)
  module Count : sig
    type t

    val create : pointers:int -> t
    (* [create ~pointers] is a new counter for a segment.
       Initially there are no cancelled cells. *)

    val removed : t -> bool
    (* [removed t] is true if a segment with this count should be removed
       (i.e. all cells are cancelled and it has no pointers).
       Once this returns [true], it will always return [true] in future. *)

    val incr_cancelled : t -> bool
    (* Increment the count of cancelled cells, then return [removed t] for the new state. *)

    val try_inc_pointers : t -> bool
    (* Atomically increment the pointers count, unless [removed t].
       Returns [true] on success. *)

    val dec_pointers : t -> bool
    (* Decrement the pointers count, then return [removed t] for the new state. *)

    val validate : expected_pointers:int -> t -> unit
    (* [validate ~expected_pointers t] check that [t] is a valid count for a non-removed segment. *)

    val dump : t Fmt.t
  end = struct
    type t = int Atomic.t

    (* We use 16 bits for the cancelled count, which should be plenty.
       The remaining bits (at least 15) are used for the pointer count,
       which normally doesn't go above 2 (except temporarily, and limited
       by the number of domains). *)
    let () = assert (cells_per_segment < 0x10000)

    let v ~pointers ~cancelled = (pointers lsl 16) lor cancelled
    let v_removed = v ~pointers:0 ~cancelled:cells_per_segment
    let pointers v = v lsr 16
    let cancelled v = v land 0xffff

    let create ~pointers = Atomic.make (v ~pointers ~cancelled:0)

    let dump f t =
      let v = Atomic.get t in
      Fmt.pf f "pointers=%d, cancelled=%d" (pointers v) (cancelled v)

    let incr_cancelled t =
      Atomic.fetch_and_add t 1 = v_removed - 1

    let rec try_inc_pointers t =
      let v = Atomic.get t in
      if v = v_removed then false
      else (
        if Atomic.compare_and_set t v (v + (1 lsl 16)) then true
        else try_inc_pointers t
      )

    let dec_pointers t =
      Atomic.fetch_and_add t (-1 lsl 16) = v_removed + (1 lsl 16)

    let removed t =
      Atomic.get t = v_removed

    let validate ~expected_pointers t =
      let v = Atomic.get t in
      assert (cancelled v >= 0 && cancelled v <= cells_per_segment);
      if cancelled v = cells_per_segment then assert (pointers v > 0);
      if pointers v <> expected_pointers then
        Fmt.failwith "Bad pointer count!"
  end

  (* A segment is a node in a linked list containing an array of [cells_per_segment] cells. *)
  module Segment : sig
    type 'a t

    val make_init : unit -> 'a t
    (* [make_init ()] is a new initial segment. *)

    val id : _ t -> Index.segment_id

    val get : 'a t -> int -> 'a Cell.t Atomic.t
    (* [get t offset] is the cell at [offset] within [t]. *)

    val try_inc_pointers : _ t -> bool
    (* Atomically increment the pointers count if the segment isn't removed.
       Returns [true] on success, or [false] if the segment was removed first. *)

    val dec_pointers : _ t -> unit
    (* Decrement the pointers count, removing the segment if it is no longer
       needed. *)

    val find : 'a t -> Index.segment_id -> 'a t
    (* [find t id] finds the segment [id] searching forwards from [t].

       If the target segment has not yet been created, this creates it (atomically).
       If the target segment has been removed, this returns the next non-removed segment. *)

    val clear_prev : _ t -> unit
    (* Called when the resumer has reached this segment,
       so it will never need to skip over any previous segments.
       Therefore, the previous pointer is no longer required and
       previous segments can be GC'd. *)

    val cancel_cell : _ t -> unit
    (* Increment the cancelled-cells counter, and remove the segment if it is no longer useful. *)

    val validate : 'a t -> suspend:'a t -> resume:'a t -> unit
    (* [validate t ~suspend ~resume] checks that [t] is in a valid state,
       assuming there are no operations currently in progress.
       [suspend] and [resume] are the segments of the suspend and resume pointers.
       It checks that both are reachable from [t]. *)

    val dump_list : label:Index.t Fmt.t -> 'a t Fmt.t
    (* [dump_list] formats this segment and all following ones for debugging.
       @param label Used to annotate indexes. *)
  end = struct
    type 'a t = {
      id : Index.segment_id;
      count : Count.t;
      cells : 'a Cell.t Atomic.t array;
      prev : 'a t option Atomic.t;      (* None if first, or [prev] is no longer needed *)
      next : 'a t option Atomic.t;      (* None if not yet created *)
    }

    let id t = t.id

    let get t i = Array.get t.cells i

    let pp_id f t = Int63.pp f t.id

    let dump_cells ~label f t =
      let idx = ref (Index.of_segment t.id) in
      for i = 0 to Array.length t.cells - 1 do
        Fmt.pf f "@,%a" Cell.dump (Atomic.get t.cells.(i));
        label f !idx;
        idx := Index.succ !idx
      done

    let rec dump_list ~label f t =
      Fmt.pf f "@[<v2>Segment %a (prev=%a, %a):%a@]"
        pp_id t
        (Fmt.Dump.option pp_id) (Atomic.get t.prev)
        Count.dump t.count
        (dump_cells ~label) t;
      let next = Atomic.get t.next in
      begin match next with
        | Some next when next.id = Int63.succ t.id ->
          ()       (* We'll show the labels at the start of the next segment *)
        | _ ->
          Fmt.pf f "@,End%a"
            label (Index.of_segment (Int63.succ t.id))
      end;
      Option.iter (fun next -> Fmt.cut f (); dump_list ~label f next) next

    let next t =
      match Atomic.get t.next with
      | Some s -> s
      | None ->
        let next = {
          id = Int63.succ t.id;
          count = Count.create ~pointers:0;
          cells = Array.init cells_per_segment (fun (_ : int) -> Atomic.make Cell.init);
          next = Atomic.make None;
          prev = Atomic.make (Some t);
        } in
        if Atomic.compare_and_set t.next None (Some next) then next
        else Atomic.get t.next |> Option.get

    let removed t =
      Count.removed t.count

    (* Get the previous non-removed segment, if any. *)
    let rec alive_prev t =
      match Atomic.get t.prev with
      | Some prev when removed prev -> alive_prev prev
      | x -> x

    (* Get the next non-removed segment. *)
    let alive_next t =
      let next = Atomic.get t.next |> Option.get in
      let rec live x =
        if removed x then (
          match Atomic.get x.next with
          | Some next -> live next
          | None -> x  (* The paper says to return "tail if all are removed", but can that ever happen? *)
        ) else x
      in
      live next

    (* Remove [t] from the linked-list by splicing together
       the previous live segment before us to the next live one afterwards.
       The tricky case is when two adjacent segments get removed at the same time.
       If that happens, the next and prev lists will still always be valid
       (i.e. will include all live segments, in the correct order), but may not be optimal.
       However, we will detect that case when it happens and fix it up immediately. *)
    let rec remove t =
      if Atomic.get t.next = None then () (* Can't remove tail. This shouldn't happen anyway. *)
      else (
        let prev = alive_prev t
        and next = alive_next t in
        (* [prev] might have been removed by the time we do this, but it doesn't matter,
           we're still only skipping removed segments (just not as many as desired).
           We'll fix it up afterwards in that case. *)
        Atomic.set next.prev prev;
        (* Likewise [next] might have been removed too by now, but we'll correct later. *)
        Option.iter (fun prev -> Atomic.set prev.next (Some next)) prev;
        (* If either got removed by now, start again. *)
        if removed next && Atomic.get next.next <> None then remove t
        else match prev with
          | Some prev when removed prev -> remove t
          | _ -> ()
      )

    let try_inc_pointers t =
      Count.try_inc_pointers t.count

    let dec_pointers t =
      if Count.dec_pointers t.count then remove t

    let cancel_cell t =
      if Count.incr_cancelled t.count then remove t

    let rec find start id =
      if start.id >= id && not (removed start) then start
      else find (next start) id

    let make_init () =
      {
        id = Int63.zero;
        count = Count.create ~pointers:2;
        cells = Array.init cells_per_segment (fun (_ : int) -> Atomic.make Cell.init);
        next = Atomic.make None;
        prev = Atomic.make None;
      }

    (* Note: this assumes the system is at rest (no operations in progress). *)
    let rec validate t ~suspend ~resume ~seen_pointers =
      let expected_pointers =
        (if t == suspend then 1 else 0) +
        (if t == resume then 1 else 0)
      in
      Count.validate ~expected_pointers t.count;
      let seen_pointers = seen_pointers + expected_pointers in
      match Atomic.get t.next with
      | None -> assert (seen_pointers = 2)
      | Some next ->
        begin match Atomic.get next.prev with
          | None -> assert (resume.id >= next.id)
          | Some t2 -> assert (resume.id < next.id && t == t2)
        end;
        validate next ~suspend ~resume ~seen_pointers

    let validate = validate ~seen_pointers:0

    let clear_prev t =
      Atomic.set t.prev None
  end

  (* A mutable pointer into the list of cells. *)
  module Position : sig
    type 'a t

    val of_segment : 'a Segment.t -> 'a t
    (* [of_segment x] is a pointer to the first cell in [x]. *)

    val next : clear_prev:bool -> 'a t -> 'a Segment.t * 'a Cell.t Atomic.t
    (* [next t ~clear_prev] returns the segment and cell of [t], and atomically increments it.
       If [t]'s segment is all cancelled and no longer exists it will skip it and retry.
       If [clear_prev] then the previous pointer is no longer required. *)

    val resume_all : 'a t -> stop:Index.t -> ('a Cell.t Atomic.t -> unit) -> unit
    (* [resume_all t ~stop f] advances [t] to [stop], then calls [f cell] on each cell advanced over. *)

    val index : _ t -> Index.t
    (* [index t] is the index of the cell currently pointed-to by [t]. *)

    val segment : 'a t -> 'a Segment.t
    (* For debugging only. The segment containing the previously-returned cell (or the initial segment),
       when the system is at rest. *)
  end = struct
    type 'a t = {
      segment : 'a Segment.t Atomic.t;  (* Note: can lag [idx] *)
      idx : Index.t Atomic.t;
    }

    let segment t = Atomic.get t.segment
    let index t = Atomic.get t.idx

    let of_segment segment =
      {
        segment = Atomic.make segment;
        idx = Atomic.make Index.zero;
      }

    (* Set [t.segment] to [target] if [target] is ahead of us.
       Returns [false] if [target] gets removed first. *)
    let rec move_forward t (target : _ Segment.t) =
      let cur = Atomic.get t.segment in
      if Segment.id cur >= Segment.id target then true
      else (
        if not (Segment.try_inc_pointers target) then false     (* target already removed *)
        else (
          if Atomic.compare_and_set t.segment cur target then (
            Segment.dec_pointers cur;
            true
          ) else (
            (* Concurrent update of [t]. Undo ref-count changes and retry. *)
            Segment.dec_pointers target;
            move_forward t target
          )
        )
      )

    (* Update [t] to the segment [id] (or the next non-removed segment after it). *)
    let rec find_and_move_forward t start id =
      let target = Segment.find start id in
      if move_forward t target then target
      else find_and_move_forward t start id     (* Removed before we could increase the ref-count; rety *)

    let rec next ~clear_prev t =
      (* Get the segment first before the index. Even if [idx] moves forwards after this,
         we'll still be able to reach it from [r]. *)
      let r = Atomic.get t.segment in
      let i = Index.next t.idx in
      let id = Index.segment i in
      let s = find_and_move_forward t r id in
      if clear_prev then Segment.clear_prev s;
      if Segment.id s = id then (
        (s, Segment.get s (Index.offset i))
      ) else (
        (* The segment we wanted contains only cancelled cells.
           Try to update the index to jump over those cells, then retry. *)
        let s_index = Index.of_segment (Segment.id s) in
        ignore (Atomic.compare_and_set t.idx (Index.succ i) s_index : bool);
        next ~clear_prev t
      )

    let rec resume_all t ~stop f =
      (* Get the segment first before the index. Even if [idx] moves forwards after this,
         we'll still be able to reach it from [start_seg]. *)
      let start_seg = Atomic.get t.segment in
      let start = Atomic.get t.idx in
      if start >= stop then ()
      else if not (Atomic.compare_and_set t.idx start stop) then (
        resume_all t ~stop f
      ) else (
        (* We are now responsible for resuming all cells from [start] to [stop]. *)
        (* Move [t.segment] forward so we can free older segments now. *)
        ignore (find_and_move_forward t start_seg (Index.segment (Index.pred stop)) : _ Segment.t);
        (* Resume all cells from [i] to [stop] (reachable via [seg]): *)
        let rec aux seg i =
          if i < stop then (
            let seg = Segment.find seg (Index.segment i) in
            Segment.clear_prev seg;
            let seg_start = Index.of_segment (Segment.id seg) in
            if seg_start < stop then (
              let i = max i seg_start in
              f (Segment.get seg (Index.offset i));
              aux seg (Index.succ i)
            )
          )
        in
        aux start_seg start
      )
  end

  type 'a t = {
    resume : 'a Position.t;
    suspend : 'a Position.t;
  }

  type 'a segment = 'a Segment.t

  let next_suspend t =
    Position.next t.suspend ~clear_prev:false

  let next_resume t =
    snd @@ Position.next t.resume ~clear_prev:true

  let resume_all t f =
    Position.resume_all t.resume ~stop:(Position.index t.suspend) f

  let cancel_cell = Segment.cancel_cell

  let make () =
    let init = Segment.make_init () in
    {
      resume = Position.of_segment init;
      suspend = Position.of_segment init;
    }

  let validate t =
    let suspend = Position.segment t.suspend in
    let resume = Position.segment t.resume in
    let start =
      if Segment.id suspend < Segment.id resume then suspend
      else resume
    in
    Segment.validate start ~suspend ~resume

  let dump f t =
    let suspend = Position.index t.suspend in
    let resume = Position.index t.resume in
    let start =
      if suspend < resume then t.suspend
      else t.resume
    in
    let label f i =
      if i = suspend then Format.pp_print_string f " (suspend)";
      if i = resume then Format.pp_print_string f " (resume)";
    in
    Format.fprintf f "@[<v>%a@]" (Segment.dump_list ~label) (Position.segment start)
end
