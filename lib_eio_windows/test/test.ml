open Eio.Std

module Timeout = struct
  let test clock () =
    let t0 = Unix.gettimeofday () in
    Eio.Time.sleep clock 0.01;
    let t1 = Unix.gettimeofday () in
    let diff = t1 -. t0 in
    if diff >= 0.01 then () else Alcotest.failf "Expected bigger difference than %f" diff


  let tests = [
    "timeout", `Quick, (fun env -> test env#clock)
  ]
end

module Random = struct
  let test_random env () =
    let src = Eio.Stdenv.secure_random env in
    let b1 = Cstruct.create 8 in
    let b2 = Cstruct.create 8 in
    Eio.Flow.read_exact src b1;
    Eio.Flow.read_exact src b2;
    Alcotest.(check bool) "different random" (not (Cstruct.equal b1 b2)) true

  let tests = [
    "different", `Quick, test_random
  ]
end

module Dla = struct

  let test_dla () =
    let open Kcas in
    let x = Loc.make 0 in
    let y = Loc.make 0 in
    let foreign_domain = Domain.spawn @@ fun () ->
      let x = Loc.get_as (fun x -> Retry.unless (x <> 0); x) x in
      Loc.set y 22;
      x
    in
    Loc.set x 20;
    let y' = Loc.get_as (fun y -> Retry.unless (y <> 0); y) y in
    Alcotest.(check int) "correct y" y' 22;
    let ans = y' + Domain.join foreign_domain in
    Alcotest.(check int) "answer" ans 42

  let tests = [
    "dla", `Quick, (fun _ -> test_dla)
  ]
end

module Await_fd = struct
  let test_cancel () =
    let a, b = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
    (* Start awaiting readable/writable state, but cancel immediately. *)
    try
      Eio.Cancel.sub (fun cc ->
          Fiber.all [
            (fun () -> Eio_unix.await_readable a);
            (fun () -> Eio_unix.await_writable b);
            (fun () -> Eio.Cancel.cancel cc Exit);
          ];
          assert false
        )
    with Eio.Cancel.Cancelled _ ->
      (* Now wait for something else. Will fail if the old FDs are still being waited on. *)
      let c, d = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
      Unix.close a;
      Unix.close b;
      Fiber.first
        (fun () -> Eio_unix.await_readable c)
        (fun () -> Eio_unix.await_writable d);
      Unix.close c;
      Unix.close d

  let tests = [
    "cancel", `Quick, (fun _ -> test_cancel);
  ]
end


let write_prefix () =
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.Net.socketpair_stream ~sw () in
  let byte = Cstruct.of_string "x" in
  let bufs = Cstruct.empty :: List.init 32 (fun _ -> byte) in
  let sent, received = Fiber.pair
      (fun () -> Eio.Flow.single_write w bufs)
      (fun () ->
         Eio.Flow.read_exact r (Cstruct.create 1);
         Eio.Flow.shutdown w `Send;
         let rest = Eio.Buf_read.(parse_exn take_all) r ~max_size:64 in
         1 + String.length rest)
  in
  Alcotest.(check int) "reported bytes" received sent

let () =
  let run (name, speed, test) =
    name, speed, (fun () -> Eio_windows.run (fun env -> test env ()))
  in
  let suites = [
    "flow", ["single_write reports its prefix", `Quick, (fun _ -> write_prefix)];
    "net", Test_net.tests ();
    "fs", Test_fs.tests ();
    "process", Test_process.tests ();
    "timeout", Timeout.tests;
    "random", Random.tests;
    "dla", Dla.tests;
    "await", Await_fd.tests;
  ] in
  Alcotest.run "eio_windows" (List.map (fun (name, tests) -> name, List.map run tests) suites)
