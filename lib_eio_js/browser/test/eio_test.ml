open Eio

(* Filesystem tests *)
module Fs = struct
  let ( / ) = Path.(/)
  let test_read_write env () =
    let test_file = "test.txt" in
    let text = "abcdefgh" in
    let len = String.length text in
    let _write =
      Path.with_open_out ~create:(`If_missing 0o644) (env#fs / test_file) @@ fun sink ->
      Flow.copy_string text sink
    in
    let buf = Cstruct.create len in
    let _read =
      Path.with_open_in (env#fs / test_file) @@ fun source ->
      Flow.read_exact source buf
    in
    Path.unlink (env#fs / test_file);
    Alcotest.(check string) "same string" text (Cstruct.to_string buf)

  let test_append env () =
    let test_file = "test.txt" in
    let text = "abcdefgh" in
    let len = String.length text in
    let _write =
      Path.with_open_out ~create:(`If_missing 0o644) (env#fs / test_file) @@ fun sink ->
      Flow.copy_string text sink
    in
    let _write2 =
      Path.with_open_out ~append:true ~create:(`If_missing 0o644) (env#fs / test_file) @@ fun sink ->
      Flow.copy_string text sink
    in
    let buf = Cstruct.create (2 * len) in
    let _read =
      Path.with_open_in (env#fs / test_file) @@ fun source ->
      Flow.read_exact source buf
    in
    Path.unlink (env#fs / test_file);
    Alcotest.(check string) "same string" (text ^ text) (Cstruct.to_string buf)

  let optint_int63 = Alcotest.testable Optint.Int63.pp Optint.Int63.equal

  let test_fails_if_exists env () =
    let test_file = "./test.txt" in
    let text = "abcdefgh" in
    let _write =
      Path.with_open_out ~create:(`If_missing 0o644) (env#fs / test_file) @@ fun sink ->
      Flow.copy_string text sink
    in
    Fun.protect
      (fun () ->
         try
           Path.with_open_out ~create:(`Exclusive 0o644) (env#fs / test_file) @@ fun _ ->
           Alcotest.failf "Expected to fail with already exists"
         with
         | Eio.Exn.Io (Eio.Fs.E (Fs.Already_exists _), _) -> ()
         | exn -> Alcotest.failf "Expected already exists exception, got: %a" Fmt.exn exn
      ) ~finally:(fun () -> Path.unlink (env#fs / test_file))

  let test_read_write env () =
    let test_file = "test.txt" in
    let text = "abcdefgh" in
    let len = String.length text in
    let _write =
      Path.with_open_out ~create:(`If_missing 0o644) (env#fs / test_file) @@ fun sink ->
      Flow.copy_string text sink
    in
    let stat =
      Eio.Switch.run @@ fun sw ->
      let fd = Path.open_in ~sw (env#fs / test_file) in
      File.stat fd
    in
    let is_file = stat.kind = `Regular_file in
    Alcotest.(check bool) "is file" true is_file;
    Alcotest.(check optint_int63) "same size" (Optint.Int63.of_int len) stat.size;
    Path.unlink (env#fs / test_file)

  let tests env = [
    Alcotest.test_case "read and write" `Quick (test_read_write env);
    Alcotest.test_case "append" `Quick (test_append env);
    Alcotest.test_case "fail if exists" `Quick (test_fails_if_exists env);
    Alcotest.test_case "fstat" `Quick (test_fails_if_exists env);
  ]
end

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
    Alcotest.test_case "fiber first cancel" `Quick test_simple_cancel;
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