open Fibreslib

let () =
  Logs.(set_level ~all:true (Some Debug));
  Logs.set_reporter @@ Logs.format_reporter ();
  Printexc.record_backtrace true

let state t =
  Alcotest.of_pp
    (fun f -> function
       | `Unresolved -> Fmt.string f "unresolved"
       | `Broken (Failure msg) -> Fmt.pf f "broken:%s" msg
       | `Broken ex -> Fmt.pf f "broken:%a" Fmt.exn ex
       | `Fulfilled x -> Fmt.pf f "fulfilled:%a" (Alcotest.pp t) x
    )

let get_state p =
  match Promise.state p with
  | Unresolved _ -> `Unresolved
  | Broken ex -> `Broken ex
  | Fulfilled x -> `Fulfilled x

let test_promise () =
  Eunix.run @@ fun () ->
  let p, r = Promise.create () in
  Alcotest.(check (state string)) "Initially unresolved" (get_state p) `Unresolved;
  let thread = Fibre.fork (fun () -> Promise.await p) in
  Promise.fulfill r "ok";
  Alcotest.(check (state string)) "Resolved OK" (get_state p) (`Fulfilled "ok");
  Alcotest.(check (state string)) "Thread unresolved" (get_state thread) `Unresolved;
  Fibre.yield ();
  Alcotest.(check (state string)) "Thread resolved" (get_state thread) @@ `Fulfilled "ok";
  let result = Promise.await thread in
  Alcotest.(check string) "Await result" result "ok"

let test_promise_exn () =
  Eunix.run @@ fun () ->
  let p, r = Promise.create () in
  Alcotest.(check (state reject)) "Initially unresolved" (get_state p) `Unresolved;
  let thread = Fibre.fork (fun () -> Promise.await p) in
  Promise.break r (Failure "test");
  Alcotest.(check (state reject)) "Broken" (get_state p) @@ `Broken (Failure "test");
  Alcotest.(check (state reject)) "Thread unresolved" (get_state thread) `Unresolved;
  Fibre.yield ();
  Alcotest.(check (state reject)) "Thread broken" (get_state thread) @@ `Broken (Failure "test");
  match Promise.await thread with
  | `Cant_happen -> assert false
  | exception (Failure msg) -> Alcotest.(check string) "Await result" msg "test"

let read_one_byte r =
  Fibre.fork (fun () ->
      Eunix.await_readable (Eunix.FD.of_unix r);
      let b = Bytes.create 1 in
      let got = Unix.read r b 0 1 in
      assert (got = 1);
      Bytes.to_string b
    )

let test_poll_add () =
  Eunix.run @@ fun () ->
  let r, w = Unix.pipe () in
  let thread = read_one_byte r in
  Fibre.yield ();
  Eunix.await_writable (Eunix.FD.of_unix w);
  let sent = Unix.write w (Bytes.of_string "!") 0 1 in
  assert (sent = 1);
  let result = Promise.await thread in
  Alcotest.(check string) "Received data" "!" result

let test_poll_add_busy () =
  Eunix.run ~queue_depth:1 @@ fun () ->
  let r, w = Unix.pipe () in
  let a = read_one_byte r in
  let b = read_one_byte r in
  Fibre.yield ();
  let sent = Unix.write w (Bytes.of_string "!!") 0 2 in
  assert (sent = 2);
  let a = Promise.await a in
  Alcotest.(check string) "Received data" "!" a;
  let b = Promise.await b in
  Alcotest.(check string) "Received data" "!" b

let test_fork_detach () =
  Eunix.run ~queue_depth:1 @@ fun () ->
  let i = ref 0 in
  Fibre.fork_detach (fun () -> incr i) ~on_error:raise;
  Alcotest.(check int) "Forked code ran" 1 !i;
  let p1, r1 = Promise.create () in
  let p2, r2 = Promise.create () in
  Fibre.fork_detach (fun () -> Promise.await p1; incr i; raise Exit) ~on_error:(Promise.fulfill r2);
  Alcotest.(check int) "Forked code waiting" 1 !i;
  Promise.fulfill r1 ();
  let result = Promise.await p2 in
  Alcotest.(check int) "Forked code ran" 2 !i;
  Alcotest.(check bool) "Error handled" true (result = Exit)

let test_semaphore () =
  Eunix.run ~queue_depth:1 @@ fun () ->
  let running = ref 0 in
  let sem = Semaphore.make 2 in
  let a = Fibre.fork (fun () -> Ctf.label "a"; Semaphore.acquire sem; incr running) in
  let b = Fibre.fork (fun () -> Ctf.label "b"; Semaphore.acquire sem; incr running) in
  let c = Fibre.fork (fun () -> Ctf.label "c"; Semaphore.acquire sem; incr running) in
  let d = Fibre.fork (fun () -> Ctf.label "d"; Semaphore.acquire sem; incr running) in
  Alcotest.(check int) "Two running" 2 !running;
  Promise.await a;
  Promise.await b;
  (* a finishes and c starts *)
  decr running;
  Semaphore.release sem;
  Alcotest.(check int) "One finished " 1 !running;
  Fibre.yield ();
  Alcotest.(check int) "Two running again" 2 !running;
  Promise.await c;
  (* b finishes and d starts *)
  decr running;
  Semaphore.release sem;
  Promise.await d;
  decr running;
  Semaphore.release sem;
  decr running;
  Semaphore.release sem

let test_semaphore_no_waiter () =
  Eunix.run ~queue_depth:2 @@ fun () ->
  let sem = Semaphore.make 0 in
  Semaphore.release sem;        (* Release with free-counter *)
  Alcotest.(check int) "Initial config" 1 (Semaphore.get_value sem);
  Fibre.fork_detach (fun () -> Ctf.label "a"; Semaphore.acquire sem) ~on_error:raise;
  Fibre.fork_detach (fun () -> Ctf.label "b"; Semaphore.acquire sem) ~on_error:raise;
  Alcotest.(check int) "A running" 0 (Semaphore.get_value sem);
  Semaphore.release sem;        (* Release with a non-empty wait-queue *)
  Alcotest.(check int) "Now b running" 0 (Semaphore.get_value sem);
  Semaphore.release sem;        (* Release with an empty wait-queue *)
  Alcotest.(check int) "Finished" 1 (Semaphore.get_value sem)

let () =
  let open Alcotest in
  run "eioio" [
    "promise", [
      test_case "promise"      `Quick test_promise;
      test_case "promise_exn"  `Quick test_promise_exn;
    ];
    "semaphore", [
      test_case "semaphore"    `Quick test_semaphore;
      test_case "no-waiter"    `Quick test_semaphore_no_waiter;
    ];
    "fork", [
      test_case "fork_detach"  `Quick test_fork_detach;
    ];
    "io", [
      test_case "poll_add"      `Quick test_poll_add;
      test_case "poll_add_busy" `Quick test_poll_add_busy;
    ];
  ]
