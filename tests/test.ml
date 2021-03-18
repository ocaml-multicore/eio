open Eunix

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
  let thread = Eunix.fork (fun () -> Promise.await p) in
  Promise.fulfill r "ok";
  Alcotest.(check (state string)) "Resolved OK" (get_state p) (`Fulfilled "ok");
  Alcotest.(check (state string)) "Thread unresolved" (get_state thread) `Unresolved;
  yield ();
  Alcotest.(check (state string)) "Thread resolved" (get_state thread) @@ `Fulfilled "ok";
  let result = Promise.await thread in
  Alcotest.(check string) "Await result" result "ok"

let test_promise_exn () =
  Eunix.run @@ fun () ->
  let p, r = Promise.create () in
  Alcotest.(check (state reject)) "Initially unresolved" (get_state p) `Unresolved;
  let thread = Eunix.fork (fun () -> Promise.await p) in
  Promise.break r (Failure "test");
  Alcotest.(check (state reject)) "Broken" (get_state p) @@ `Broken (Failure "test");
  Alcotest.(check (state reject)) "Thread unresolved" (get_state thread) `Unresolved;
  yield ();
  Alcotest.(check (state reject)) "Thread broken" (get_state thread) @@ `Broken (Failure "test");
  match Promise.await thread with
  | `Cant_happen -> assert false
  | exception (Failure msg) -> Alcotest.(check string) "Await result" msg "test"

let read_one_byte r =
  Eunix.fork (fun () ->
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
  Eunix.yield ();
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
  Eunix.yield ();
  let sent = Unix.write w (Bytes.of_string "!!") 0 2 in
  assert (sent = 2);
  let a = Promise.await a in
  Alcotest.(check string) "Received data" "!" a;
  let b = Promise.await b in
  Alcotest.(check string) "Received data" "!" b

let () =
  let open Alcotest in
  run "eioio" [
    "promise", [
      test_case "promise"      `Quick test_promise;
      test_case "promise_exn"  `Quick test_promise_exn;
    ];
    "io", [
      test_case "poll_add"      `Quick test_poll_add;
      test_case "poll_add_busy" `Quick test_poll_add_busy;
    ];
  ]
