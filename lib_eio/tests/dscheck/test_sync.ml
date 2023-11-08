let debug = false

module T = Sync

(* Create a synchronous channel. [prod] producers write values to it and [cons] consumers take values.
   Both producers and consumers try to cancel if they can.
   [take_nonblocking] additional consumers also perform a single non-blocking take.
   At the end, we check that:
   - We received the expected values.
   - No processes are still queued up (since everything tries to cancel before finishing).
 *)
let test ~prod ~cons ~take_nonblocking () =
  let messages = ref [] in
  let log fmt = (fmt ^^ "@.") |> Format.kasprintf @@ fun msg -> messages := msg :: !messages in
  if debug then log "== start ==";
  let t = T.create () in
  let finished_producers = ref 0 in
  let expected_total = ref 0 in
  let received = ref 0 in
  let cancelled_consumers = ref 0 in
  let cancelled_producers = ref 0 in
  let run_consumer l =
    Fake_sched.run
      (fun () ->
         match T.take t with
         | Error `Closed -> assert false
         | Ok v ->
           if debug then log "c%d: Recv %d" l v;
           received := !received + v
         | exception Eio__core.Cancel.Cancelled _ ->
           if debug then log "c%d: Cancelled" l;
           incr cancelled_consumers
      )
    |> Option.iter (fun ctx ->
        if debug then log "c%d: Suspended" l;
        Fake_sched.cancel ctx;
      )
  in
  let run_producer v =
    Fake_sched.run
      (fun () ->
         match T.put t v with
         | () ->
           if debug then log "p%d: Sent" v;
           expected_total := !expected_total + v;
           incr finished_producers
         | exception Eio__core.Cancel.Cancelled _ ->
           if debug then log "p%d: Cancelled" v;
           incr finished_producers;
           incr cancelled_producers
      )
    |> Option.iter (fun ctx ->
      if debug then log "p%d: Suspended sending" v;
      Fake_sched.cancel ctx
    )
  in
  for i = 1 to prod do
    Atomic.spawn (fun () -> run_producer i)
  done;
  for i = 1 to cons do
    Atomic.spawn (fun () -> run_consumer i)
  done;
  for i = 1 to take_nonblocking do
    Atomic.spawn (fun () ->
        match T.take_nonblocking t with
        | Error `Closed -> assert false
        | Error `Would_block ->
          if debug then log "nb%d: found nothing" i;
          incr cancelled_consumers;
        | Ok v ->
          if debug then log "nb%d: took %d" i v;
          received := !received + v
      )
  done;
  Atomic.final (fun () ->
      if debug then (
        List.iter print_string (List.rev !messages);
        Fmt.pr "%a@." T.dump t;
        Fmt.pr "Received total = %d/%d (%d/%d cancelled consumers)@."
          !received !expected_total
          !cancelled_consumers (cons + take_nonblocking);
        Fmt.pr "Finished producers = %d/%d (incl %d cancelled)@."
          !finished_producers prod
          !cancelled_producers;
      );
      assert (!finished_producers = prod);
      (* Everyone finishes by trying to cancel (if they didn't succeed immediately),
         so there shouldn't be any balance at the end. *)
      assert (T.balance t = Ok 0);
      assert (!received = !expected_total);
    )

(* A producer puts "A" and then closes the stream.
   Two consumers try to read. One gets the "A", the other gets end-of-stream. *)
let test_close () =
  let t = T.create () in
  let got = ref [] in
  Atomic.spawn
    (fun () ->
       let _ : Sync.Cancel.t option = Fake_sched.run (fun () -> T.put t "A"; T.close t) in
       ()
    );
  for _ = 1 to 2 do
    Atomic.spawn
      (fun () ->
         let _ : Sync.Cancel.t option = Fake_sched.run (fun () ->
             let msg = T.take t |> Result.value ~default:"end-of-stream" in
             got := msg :: !got
           )
         in
         ()
      );
  done;
  Atomic.final (fun () ->
      let results = List.sort String.compare !got in
      if debug then (
        Fmt.pr "%a@." T.dump t;
        Fmt.pr "%a@." Fmt.(Dump.list string) results;
      );
      assert (results = ["A"; "end-of-stream"]);
      assert (T.balance t = Error `Closed);
    )

(* A producer tries to add an item (but never succeeds, as there are no consumers).
   At some point, the stream is closed and the operation aborts. *)
let test_close2 () =
  let t = T.create () in
  let result = ref "Waiting" in
  Atomic.spawn
    (fun () ->
       let _ : Sync.Cancel.t option = Fake_sched.run (fun () ->
           match T.put t "A" with
           | () -> failwith "Shouldn't succeed with no consumer!"
           | exception (Invalid_argument msg) -> result := msg
         ) in
       ()
    );
  Atomic.spawn (fun () -> T.close t);
  Atomic.final (fun () ->
      if debug then (
        Fmt.pr "%a@." T.dump t;
        Fmt.pr "%s@." !result;
      );
      match !result with
      | "Stream closed" -> ()
      | x -> failwith x
    )

let () =
  Atomic.trace (test ~prod:1 ~cons:1 ~take_nonblocking:1);
  Atomic.trace (test ~prod:2 ~cons:1 ~take_nonblocking:0);
  Atomic.trace (test ~prod:1 ~cons:2 ~take_nonblocking:0);
  Atomic.trace test_close;
  Atomic.trace test_close2;
