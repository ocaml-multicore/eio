let debug = false

(* For each of [n_values], spawn one producer and one consumer.
   If the consumer has to wait, it will also try to cancel.
   The consumer increases the total if it gets a value;
   the producer increments it if the cancellation succeeds instead. *)
let test_cells ~segment_order ~n_values () =
  let module Cqs = Simple_cqs.Make(struct let segment_order = segment_order end) in
  let expected_total = n_values in
  let t = Cqs.make () in
  let total = ref 0 in
  for i = 1 to n_values do
    Atomic.spawn (fun () ->
        if debug then Fmt.epr "%d: wrote value@." i;
        Cqs.resume t 1;
      );
    Atomic.spawn (fun () ->
        match
          Cqs.suspend t (fun v ->
              if debug then Fmt.epr "%d: resumed@." i;
              total := !total + v
            )
        with
        | None -> () (* Already resumed *)
        | Some request ->
          if Cqs.cancel request then (
            if debug then Fmt.epr "%d: cancelled@." i;
            total := !total + 1
          )
      );
  done;
  Atomic.final
    (fun () ->
       if debug then (
         Format.eprintf "%a@." Cqs.dump t;
         Format.eprintf "total=%d, expected_total=%d\n%!" !total expected_total;
       );
       Cqs.Cells.validate t;
       assert (!total = expected_total);
       (*        Printf.printf "total = %d\n%!" !total *)
    )

(* An even simpler cell type with no payload. Just for testing removing whole cancelled segments. *)
module Unit_cells(Config : sig val segment_order : int end) = struct
  module Cell = struct
    type _ t =
      | Empty           (* A consumer is intending to collect a value in the future *)
      | Value           (* A value is waiting for its consumer *)
      | Cancelled       (* The consumer cancelled *)

    let init = Empty

    let segment_order = Config.segment_order

    let dump f = function
      | Empty -> Fmt.string f "Empty"
      | Value -> Fmt.string f "Value"
      | Cancelled -> Fmt.string f "Cancelled"
  end
  module Cells = Cells.Make(Cell)

  type request = unit Cells.segment * unit Cell.t Atomic.t

  let cancel (segment, cell) =
    if Atomic.compare_and_set cell Cell.Empty Cancelled then (
      Cells.cancel_cell segment;
      true
    ) else false        (* Already at [Value]; cancellation fails. *)

  (* Provide a value. Returns [false] if already [Cancelled]. *)
  let resume t =
    let cell =  Cells.next_resume t in
    Atomic.compare_and_set cell Empty Value

  (* We reuse the [Empty] state to mean [Waiting]. *)
  let suspend t : request = Cells.next_suspend t

  let resume_all t =
    Cells.resume_all t

  let make = Cells.make
  let dump f t = Atomic.check (fun () -> Cells.dump f t; true)
  let validate t = Atomic.check (fun () -> Cells.validate t; true)
end

(* A producer writes [n_items] to the queue (retrying if the cell gets cancelled first).
   A consumer reads [n_items] from the queue (cancelling and retrying once if it can).
   At the end, the consumer and resumer are at the same position.
   This tests what happens if a whole segment gets cancelled and the producer therefore skips it.
   [test_cells] is too slow to test this. *)
let test_skip_segments ~segment_order ~n_items () =
  let module Cells = Unit_cells(struct let segment_order = segment_order end) in
  if debug then print_endline "== start ==";
  let t = Cells.make () in
  Atomic.spawn (fun () ->
      for _ = 1 to n_items do
        let rec loop ~may_cancel =
          if debug then print_endline "suspend";
          let request = Cells.suspend t in
          if may_cancel && Cells.cancel request then (
            if debug then print_endline "cancelled";
            loop ~may_cancel:false
          )
        in
        loop ~may_cancel:true
      done
    );
  Atomic.spawn (fun () ->
      for _ = 1 to n_items do
        if debug then print_endline "resume";
        while not (Cells.resume t) do () done
      done
    );
  Atomic.final
    (fun () ->
       if debug then Fmt.pr "%a@." Cells.dump t;
       Cells.Cells.validate t;
       assert (Cells.Cells.Position.index t.suspend =
               Cells.Cells.Position.index t.resume);
    )

(* Create a list of [n_internal + 2] segments and cancel all the internal ones.
   Ensure the list is valid afterwards.
   This is simpler than [test_skip_segments], so we can test longer sequences
   of cancellations. *)
let test_cancel_only ~n_internal () =
  let module Cells = Unit_cells(struct let segment_order = 0 end) in
  let t = Cells.make () in
  ignore (Cells.suspend t : Cells.request);
  let internals = Array.init n_internal (fun _ -> Cells.suspend t) in
  ignore (Cells.suspend t : Cells.request);
  let in_progress = ref 0 in
  for i = 0 to n_internal - 1 do
    Atomic.spawn (fun () ->
        incr in_progress;
        assert (Cells.cancel internals.(i));
        decr in_progress;
        if !in_progress = 0 then Cells.validate t
      )
  done;
  Atomic.final
    (fun () ->
       assert (Cells.resume t);
       assert (Cells.resume t);
       if debug then Fmt.pr "%a@." Cells.dump t;
       Cells.validate t;
       assert (Cells.Cells.Position.index t.suspend =
               Cells.Cells.Position.index t.resume);
    )

(* Create [n] requests. Then try to cancel them in parallel with doing a resume_all.
   Check the number of resumed requests is plausible (at least as many as there
   were requests that hadn't started cancelling, and no more than those that hadn't
   finished cancelling. *)
let test_broadcast ~segment_order ~n () =
  let messages = ref [] in
  let log fmt = (fmt ^^ "@.") |> Format.kasprintf @@ fun msg -> messages := msg :: !messages in
  if debug then log "== start ==";
  let module Cells = Unit_cells(struct let segment_order = segment_order end) in
  let t = Cells.make () in
  let requests = Array.init n (fun _ -> Cells.suspend t) in
  let min_requests = Atomic.make n in
  let max_requests = Atomic.make n in
  for i = 0 to n - 1 do
    Atomic.spawn (fun () ->
        Atomic.decr min_requests;
        if debug then log "Cancelling request";
        if Cells.cancel requests.(i) then (
          Atomic.decr max_requests;
          if debug then log "Cancelled request";
        )
      )
  done;
  Atomic.spawn (fun () ->
      if debug then log "Broadcasting";
      let max_expected = Atomic.get max_requests in
      let wakes = ref 0 in
      Cells.resume_all t (fun cell ->
          match Atomic.get cell with
          | Empty -> incr wakes
          | Cancelled -> ()
          | Value -> assert false
        );
      let min_expected = Atomic.get min_requests in
      let wakes = !wakes in
      if debug then log "Broadcast done: wakes=%d (expected=%d-%d)" wakes min_expected max_expected;
      assert (min_expected <= wakes && wakes <= max_expected)
    );
  Atomic.final (fun () ->
      if debug then (
        List.iter print_string (List.rev !messages)
      )
    )

(* These tests take about 10s on my machine, with https://github.com/ocaml-multicore/dscheck/pull/3
   However, that PR is not reliable at finding all interleavings. *)
let () =
  print_endline "Test broadcast:";
  Atomic.trace (test_broadcast ~segment_order:1 ~n:3);
  print_endline "Test cancelling segments:";
  Atomic.trace (test_cancel_only ~n_internal:3);
  print_endline "Test cancelling segments while suspending and resuming:";
  Atomic.trace (test_skip_segments ~segment_order:1 ~n_items:3);
  print_endline "Test with 1 cell per segment:";
  Atomic.trace (test_cells ~segment_order:0 ~n_values:2);
  print_endline "Test with 2 cells per segment:";
  Atomic.trace (test_cells ~segment_order:1 ~n_values:2)
