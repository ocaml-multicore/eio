open Fibreslib

let run fn =
  try
    Eunix.run @@ fun _e ->
    Switch.top fn;
    print_endline "ok"
  with Failure msg ->
    print_endline msg

let%expect_test "simple" =
  run (fun _sw ->
      traceln "Running"
    );
  [%expect {|
    Running
    ok |}]

let%expect_test "turn_off" =
  run (fun sw ->
      traceln "Running";
      Switch.turn_off sw (Failure "Cancel");
      traceln "Clean up"
    );
  [%expect {|
    Running
    Clean up
    Cancel |}]

let%expect_test "both_pass" =
  run (fun sw ->
      Fibre.both ~sw
        (fun () -> for i = 1 to 2 do traceln "i = %d" i; Fibre.yield ~sw () done)
        (fun () -> for j = 1 to 2 do traceln "j = %d" j; Fibre.yield ~sw () done)
    );
  [%expect {|
    i = 1
    j = 1
    i = 2
    j = 2
    ok |}]

let%expect_test "both1" =
  run (fun sw ->
      Fibre.both ~sw
        (fun () -> for i = 1 to 5 do traceln "i = %d" i; Fibre.yield ~sw () done)
        (fun () -> failwith "Failed")
    );
  [%expect {|
    i = 1
    Failed |}]

let%expect_test "both2" =
  run (fun sw ->
      Fibre.both ~sw
        (fun () -> Fibre.yield ~sw (); failwith "Failed")
        (fun () -> for i = 1 to 5 do traceln "i = %d" i; Fibre.yield ~sw () done)
    );
  [%expect {|
    i = 1
    Failed |}]

let%expect_test "both3" =
  run (fun sw ->
      Fibre.both ~sw (fun () -> failwith "Failed") ignore;
      traceln "Not reached"
    );
  [%expect {| Failed |}]

let%expect_test "both4" =
  run (fun sw ->
      Fibre.both ~sw ignore (fun () -> failwith "Failed");
      traceln "not reached"
    );
  [%expect {| Failed |}]

let%expect_test "both_double_failure" =
  run (fun sw ->
      Fibre.both ~sw
        (fun () -> failwith "Failed 1")
        (fun () -> failwith "Failed 2")
    );
  [%expect {|
    Failed 1 |}]

let%expect_test "initial fail" =
  run (fun sw ->
      Switch.turn_off sw (Failure "Cancel");
      Fibre.fork_ignore ~sw (fun () -> traceln "Not reached");
      traceln "Also not reached"
    );
  [%expect {| Cancel |}]

let%expect_test "switch_over" =
  let x = ref None in
  run (fun sw -> x := Some sw);
  [%expect "ok"];
  let sw = Option.get !x in
  try Switch.check sw; assert false
  with Invalid_argument msg -> print_endline msg;
  [%expect {| Switch finished! |}]

let%expect_test "cancel" =
  run (fun sw ->
      let h1 = Fibre_impl.Switch.add_cancel_hook sw (fun _ -> traceln "Cancel 1") in
      let h2 = Fibre_impl.Switch.add_cancel_hook sw (fun _ -> traceln "Cancel 2") in
      let h3 = Fibre_impl.Switch.add_cancel_hook sw (fun _ -> traceln "Cancel 3") in
      Fibre_impl.Waiters.remove_waiter h2;
      Switch.turn_off sw (Failure "Cancelled");
      let h4 = Fibre_impl.Switch.add_cancel_hook sw (fun _ -> traceln "Cancel 4") in
      Fibre_impl.Waiters.remove_waiter h1;
      Fibre_impl.Waiters.remove_waiter h3;
      Fibre_impl.Waiters.remove_waiter h4
    );
  [%expect {|
    Cancel 3
    Cancel 1
    Cancel 4
    Cancelled |}]

(* Wait for either a promise or a switch; switch cancelled first. *)
let%expect_test "cancel_waiting_1" =
  run (fun sw ->
      let p, r = Promise.create () in
      Fibre.fork_ignore ~sw (fun () -> traceln "Waiting"; Promise.await ~sw p; traceln "Resolved");
      Switch.turn_off sw (Failure "Cancelled");
      Promise.fulfill r ()
    );
  [%expect {|
    Waiting
    Cancelled |}]

(* Wait for either a promise or a switch; promise resolves first. *)
let%expect_test "cancel_waiting_2" =
  run (fun sw ->
      let p, r = Promise.create () in
      Fibre.fork_ignore ~sw (fun () -> traceln "Waiting"; Promise.await ~sw p; traceln "Resolved");
      Promise.fulfill r ();
      Fibre.yield ();
      traceln "Now cancelling...";
      Switch.turn_off sw (Failure "Cancelled")
    );
  [%expect {|
    Waiting
    Resolved
    Now cancelling...
    Cancelled |}]

(* Wait for either a promise or a switch; switch cancelled first. Result version. *)
let%expect_test "cancel_waiting_1_result" =
  run (fun sw ->
      let p, r = Promise.create () in
      Fibre.fork_ignore ~sw (fun () -> traceln "Waiting"; ignore (Promise.await_result ~sw p); traceln "Resolved");
      Switch.turn_off sw (Failure "Cancelled");
      Promise.fulfill r ()
    );
  [%expect {|
    Waiting
    Cancelled |}]

(* Wait for either a promise or a switch; promise resolves first but switch off without yielding. *)
let%expect_test "cancel_waiting_2_result" =
  run (fun sw ->
      let p, r = Promise.create () in
      Fibre.fork_ignore ~sw (fun () -> traceln "Waiting"; ignore (Promise.await_result ~sw p); traceln "Resolved");
      Promise.fulfill r ();
      traceln "Now cancelling...";
      Switch.turn_off sw (Failure "Cancelled")
    );
  [%expect {|
    Waiting
    Now cancelling...
    Cancelled |}]

(* Child switches are cancelled when the parent is cancelled. *)
let%expect_test "sub_parent_off" =
  run (fun sw ->
      let p, _ = Promise.create () in
      let on_error ex = traceln "child: %s" (Printexc.to_string ex) in
      Fibre.fork_sub_ignore ~sw ~on_error (fun sw -> traceln "Child 1"; Promise.await ~sw p);
      Fibre.fork_sub_ignore ~sw ~on_error (fun sw -> traceln "Child 2"; Promise.await ~sw p);
      Switch.turn_off sw (Failure "Cancel parent")
    );
  [%expect {|
    Child 1
    Child 2
    child: (Failure "Cancel parent")
    child: (Failure "Cancel parent")
    Cancel parent |}]

(* A child can fail independently of the parent. *)
let%expect_test "sub_child_off" =
  run (fun sw ->
      let p1, r1 = Promise.create () in
      let p2, r2 = Promise.create () in
      let on_error ex = traceln "child: %s" (Printexc.to_string ex) in
      Fibre.fork_sub_ignore ~sw ~on_error (fun sw -> traceln "Child 1"; Promise.await ~sw p1);
      Fibre.fork_sub_ignore ~sw ~on_error (fun sw -> traceln "Child 2"; Promise.await ~sw p2);
      Promise.break r1 (Failure "Child error");
      Promise.fulfill r2 ();
      Fibre.yield ~sw ();
      traceln "Parent fibre is still running"
    );
  [%expect {|
    Child 1
    Child 2
    child: (Failure "Child error")
    Parent fibre is still running
    ok |}]

(* A child can be cancelled independently of the parent. *)
let%expect_test "sub_child_cancel" =
  run (fun sw ->
      let p, _ = Promise.create () in
      let on_error ex = traceln "child: %s" (Printexc.to_string ex) in
      let child = ref None in
      Fibre.fork_sub_ignore ~sw ~on_error (fun sw ->
          traceln "Child 1";
          child := Some sw;
          Promise.await ~sw p
        );
      Switch.turn_off (Option.get !child) (Failure "Cancel child");
      Fibre.yield ~sw ();
      traceln "Parent fibre is still running"
    );
  [%expect {|
    Child 1
    child: (Failure "Cancel child")
    Parent fibre is still running
    ok |}]

(* A child error handle raises. *)
let%expect_test "sub_escape" =
  run (fun sw ->
      let p, r = Promise.create () in
      let on_error = raise in
      Fibre.fork_sub_ignore ~sw ~on_error (fun sw -> traceln "Child"; Promise.await ~sw p);
      Promise.break r (Failure "Child error escapes");
      Fibre.yield ~sw ();
      traceln "Not reached"
    );
  [%expect {|
    Child
    Child error escapes |}]

let%expect_test "sub_return" =
  run (fun sw ->
      let print ex = traceln "%s" (Printexc.to_string ex); 0 in
      let x = Switch.sub ~sw ~on_error:print (fun _sw -> failwith "Child error") in
      traceln "x = %d" x
    );
  [%expect {|
    (Failure "Child error")
    x = 0
    ok |}]

let%expect_test "deadlock" =
  run (fun sw ->
      let p, _ = Promise.create () in
      Promise.await ~sw p
    );
  [%expect {| Deadlock detected: no events scheduled but main function hasn't returned |}]
