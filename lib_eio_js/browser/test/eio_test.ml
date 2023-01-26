open Eio

module Fibers = struct
  let test_yield () =
    let expect = [1; 2] in
    let res = ref [] in
    Fiber.both
      (fun () -> Fiber.yield (); res := 1 :: !res)
      (fun () -> res := 2 :: !res);
    Alcotest.(check (list int)) "same list" expect !res

  let test_simple_cancel () =
    let res =
      Fiber.first
        (fun () -> "a")
        (fun () -> Fiber.yield (); failwith "b crashed")
    in
    Alcotest.(check string) "same string" "a" res;
    let p, _r = Promise.create () in
    let res = Fiber.first
        (fun () -> "a")
        (fun () -> Promise.await p)
    in
    Alcotest.(check string) "promise cancelled" "a" res

  let tests = [
    Alcotest.test_case "yielding" `Quick test_yield;
    Alcotest.test_case "fiber first cancel" `Quick test_simple_cancel
  ]
end

module Promises = struct

  let test_promises () =
    let p1, r1 = Promise.create () in
    let p2, r2 = Promise.create () in
    let p3, r3 = Promise.create () in
    Switch.run @@ fun sw ->
    Fiber.all [
      (fun () -> Fiber.fork ~sw (fun () -> Promise.await p1; Promise.resolve r3 ()));
      (fun () -> Fiber.fork ~sw (fun () -> Promise.await p2;));
      (fun () -> Fiber.fork ~sw (fun () -> Promise.resolve r1 (); Promise.await p3; Promise.resolve r2 ()));
    ]

  let tests = [
    Alcotest.test_case "yielding" `Quick test_promises
  ]
end

module Stream = struct
  type op = [ `Add of int | `Take of int ]

  let pp_op ppf = function
    | `Add i -> Fmt.pf ppf "add %i" i
    | `Take i -> Fmt.pf ppf "take %i" i

  let op = Alcotest.of_pp pp_op

  let test_stream () =
    let l = ref [] in
    let add s v =
      Stream.add s v;
      l := (`Add v) :: !l
    in
    let take s =
      l := (`Take (Stream.take s)) :: !l
    in
    let t = Stream.create 3 in
    add t 1;
    Fiber.both
      (fun () ->
         add t 2;
         add t 3;
         add t 4;
      )
      (fun () ->
         take t;
         take t;
         take t;
         take t
      );
    let actual = List.rev !l in
    let expected = [ `Add 1; `Add 2; `Add 3; `Take 1; `Take 2; `Take 3; `Take 4; `Add 4 ] in
    Alcotest.(check (list op)) "same sequence" expected actual

  let tests = [
    Alcotest.test_case "stream1" `Quick test_stream
  ]
end