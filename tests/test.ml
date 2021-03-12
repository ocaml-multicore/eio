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

let test_promise () =
  Eunix.run @@ fun () ->
  let p, r = Promise.create () in
  Alcotest.(check (state string)) "Initially unresolved" (Promise.state p) `Unresolved;
  let thread = Eunix.fork (fun () -> Promise.await p) in
  Promise.fulfill r "ok";
  Alcotest.(check (state string)) "Resolved OK" (Promise.state p) (`Fulfilled "ok");
  Alcotest.(check (state string)) "Thread unresolved" (Promise.state thread) `Unresolved;
  yield ();
  Alcotest.(check (state string)) "Thread resolved" (Promise.state thread) @@ `Fulfilled "ok";
  let result = Promise.await thread in
  Alcotest.(check string) "Await result" result "ok"

let test_promise_exn () =
  Eunix.run @@ fun () ->
  let p, r = Promise.create () in
  Alcotest.(check (state reject)) "Initially unresolved" (Promise.state p) `Unresolved;
  let thread = Eunix.fork (fun () -> Promise.await p) in
  Promise.break r (Failure "test");
  Alcotest.(check (state reject)) "Broken" (Promise.state p) @@ `Broken (Failure "test");
  Alcotest.(check (state reject)) "Thread unresolved" (Promise.state thread) `Unresolved;
  yield ();
  Alcotest.(check (state reject)) "Thread broken" (Promise.state thread) @@ `Broken (Failure "test");
  match Promise.await thread with
  | `Cant_happen -> assert false
  | exception (Failure msg) -> Alcotest.(check string) "Await result" msg "test"

let () =
  let open Alcotest in
  run "eioio" [
    "simple", [
      test_case "promise"      `Quick test_promise;
      test_case "promise_exn"  `Quick test_promise_exn;
    ];
  ]
