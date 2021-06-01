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
  Eunix.run @@ fun _stdenv ->
  Switch.top @@ fun sw ->
  let p, r = Promise.create () in
  Alcotest.(check (state string)) "Initially unresolved" (get_state p) `Unresolved;
  let thread = Fibre.fork ~sw ~exn_turn_off:false (fun () -> Promise.await p) in
  Promise.fulfill r "ok";
  Alcotest.(check (state string)) "Resolved OK" (get_state p) (`Fulfilled "ok");
  Alcotest.(check (state string)) "Thread unresolved" (get_state thread) `Unresolved;
  Fibre.yield ();
  Alcotest.(check (state string)) "Thread resolved" (get_state thread) @@ `Fulfilled "ok";
  let result = Promise.await thread in
  Alcotest.(check string) "Await result" result "ok"

let test_promise_exn () =
  Eunix.run @@ fun _stdenv ->
  Switch.top @@ fun sw ->
  let p, r = Promise.create () in
  Alcotest.(check (state reject)) "Initially unresolved" (get_state p) `Unresolved;
  let thread = Fibre.fork ~sw ~exn_turn_off:false (fun () -> Promise.await p) in
  Promise.break r (Failure "test");
  Alcotest.(check (state reject)) "Broken" (get_state p) @@ `Broken (Failure "test");
  Alcotest.(check (state reject)) "Thread unresolved" (get_state thread) `Unresolved;
  Fibre.yield ();
  Alcotest.(check (state reject)) "Thread broken" (get_state thread) @@ `Broken (Failure "test");
  match Promise.await thread with
  | `Cant_happen -> assert false
  | exception (Failure msg) -> Alcotest.(check string) "Await result" msg "test"

let read_one_byte ~sw r =
  Fibre.fork ~sw ~exn_turn_off:true (fun () ->
      let r = Option.get (Eunix.Objects.get_fd_opt r) in
      Eunix.await_readable r;
      let b = Bytes.create 1 in
      let got = Unix.read (Eunix.FD.to_unix r) b 0 1 in
      assert (got = 1);
      Bytes.to_string b
    )

let test_poll_add () =
  Eunix.run @@ fun _stdenv ->
  Switch.top @@ fun sw ->
  let r, w = Eunix.pipe sw in
  let thread = read_one_byte ~sw r in
  Fibre.yield ();
  let w = Option.get (Eunix.Objects.get_fd_opt w) in
  Eunix.await_writable w;
  let sent = Unix.write (Eunix.FD.to_unix w) (Bytes.of_string "!") 0 1 in
  assert (sent = 1);
  let result = Promise.await thread in
  Alcotest.(check string) "Received data" "!" result

let test_poll_add_busy () =
  Eunix.run ~queue_depth:1 @@ fun _stdenv ->
  Switch.top @@ fun sw ->
  let r, w = Eunix.pipe sw in
  let a = read_one_byte ~sw r in
  let b = read_one_byte ~sw r in
  Fibre.yield ();
  let w = Option.get (Eunix.Objects.get_fd_opt w) |> Eunix.FD.to_unix in
  let sent = Unix.write w (Bytes.of_string "!!") 0 2 in
  assert (sent = 2);
  let a = Promise.await a in
  Alcotest.(check string) "Received data" "!" a;
  let b = Promise.await b in
  Alcotest.(check string) "Received data" "!" b

let test_fork_ignore () =
  Eunix.run ~queue_depth:1 @@ fun _stdenv ->
  let i = ref 0 in
  Switch.top (fun sw ->
      Fibre.fork_ignore ~sw (fun () -> incr i);
    );
  Alcotest.(check int) "Forked code ran" 1 !i;
  let p1, r1 = Promise.create () in
  try
    Switch.top (fun sw ->
        Fibre.fork_ignore ~sw (fun () -> Promise.await p1; incr i; raise Exit);
        Alcotest.(check int) "Forked code waiting" 1 !i;
        Promise.fulfill r1 ();
      );
    assert false
  with Exit ->
    Alcotest.(check int) "Forked code ran" 2 !i

let test_semaphore () =
  Eunix.run ~queue_depth:1 @@ fun _stdenv ->
  Switch.top @@ fun sw ->
  let running = ref 0 in
  let sem = Semaphore.make 2 in
  let fork = Fibre.fork ~sw ~exn_turn_off:false in
  let a = fork (fun () -> Ctf.label "a"; Semaphore.acquire sem; incr running) in
  let b = fork (fun () -> Ctf.label "b"; Semaphore.acquire sem; incr running) in
  let c = fork (fun () -> Ctf.label "c"; Semaphore.acquire sem; incr running) in
  let d = fork (fun () -> Ctf.label "d"; Semaphore.acquire sem; incr running) in
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
  Eunix.run ~queue_depth:2 @@ fun _stdenv ->
  Switch.top @@ fun sw ->
  let sem = Semaphore.make 0 in
  Semaphore.release sem;        (* Release with free-counter *)
  Alcotest.(check int) "Initial config" 1 (Semaphore.get_value sem);
  Fibre.fork_ignore ~sw (fun () -> Ctf.label "a"; Semaphore.acquire sem);
  Fibre.fork_ignore ~sw (fun () -> Ctf.label "b"; Semaphore.acquire sem);
  Alcotest.(check int) "A running" 0 (Semaphore.get_value sem);
  Semaphore.release sem;        (* Release with a non-empty wait-queue *)
  Alcotest.(check int) "Now b running" 0 (Semaphore.get_value sem);
  Semaphore.release sem;        (* Release with an empty wait-queue *)
  Alcotest.(check int) "Finished" 1 (Semaphore.get_value sem)

(* Write a string to a pipe and read it out again. *)
let test_copy () =
  Eunix.run ~queue_depth:2 @@ fun _stdenv ->
  Switch.top @@ fun sw ->
  let msg = "Hello!" in
  let from_pipe, to_pipe = Eunix.pipe sw in
  let buffer = Buffer.create 20 in
  Fibre.both ~sw
    (fun () -> Eio.Flow.write (Eio.Flow.buffer_sink buffer) ~src:from_pipe)
    (fun () ->
       Eio.Flow.write to_pipe ~src:(Eio.Flow.string_source msg);
       Eio.Flow.write to_pipe ~src:(Eio.Flow.string_source msg);
       Eio.Flow.close to_pipe
    );
  Alcotest.(check string) "Copy correct" (msg ^ msg) (Buffer.contents buffer);
  Eio.Flow.close from_pipe

(* Write a string via 2 pipes. The copy from the 1st to 2nd pipe will be optimised and so tests a different code-path. *)
let test_direct_copy () =
  Eunix.run ~queue_depth:4 @@ fun _stdenv ->
  Switch.top @@ fun sw ->
  let msg = "Hello!" in
  let from_pipe1, to_pipe1 = Eunix.pipe sw in
  let from_pipe2, to_pipe2 = Eunix.pipe sw in
  let buffer = Buffer.create 20 in
  let to_output = Eio.Flow.buffer_sink buffer in
  Switch.top (fun sw ->
      Fibre.fork_ignore ~sw (fun () -> Ctf.label "copy1"; Eio.Flow.write ~src:from_pipe1 to_pipe2; Eio.Flow.close to_pipe2);
      Fibre.fork_ignore ~sw (fun () -> Ctf.label "copy2"; Eio.Flow.write ~src:from_pipe2 to_output);
      Eio.Flow.write to_pipe1 ~src:(Eio.Flow.string_source msg);
      Eio.Flow.close to_pipe1;
    );
  Alcotest.(check string) "Copy correct" msg (Buffer.contents buffer);
  Eio.Flow.close from_pipe1;
  Eio.Flow.close from_pipe2

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
      test_case "fork_ignore"  `Quick test_fork_ignore;
    ];
    "io", [
      test_case "copy"          `Quick test_copy;
      test_case "direct_copy"   `Quick test_direct_copy;
      test_case "poll_add"      `Quick test_poll_add;
      test_case "poll_add_busy" `Quick test_poll_add_busy;
    ];
  ]
