open Eio.Std

module Ctf = Eio.Private.Ctf

let () =
  Logs.(set_level ~all:true (Some Debug));
  Logs.set_reporter @@ Logs.format_reporter ();
  Printexc.record_backtrace true

let read_one_byte ~sw r =
  Fiber.fork_promise ~sw (fun () ->
      let r = Option.get (Eio_linux.get_fd_opt r) in
      Eio_linux.Low_level.await_readable r;
      let b = Bytes.create 1 in
      let got = Unix.read (Eio_linux.FD.to_unix `Peek r) b 0 1 in
      assert (got = 1);
      Bytes.to_string b
    )

let test_poll_add () =
  Eio_linux.run @@ fun _stdenv ->
  Switch.run @@ fun sw ->
  let r, w = Eio_linux.pipe sw in
  let thread = read_one_byte ~sw r in
  Fiber.yield ();
  let w = Option.get (Eio_linux.get_fd_opt w) in
  Eio_linux.Low_level.await_writable w;
  let sent = Unix.write (Eio_linux.FD.to_unix `Peek w) (Bytes.of_string "!") 0 1 in
  assert (sent = 1);
  let result = Promise.await_exn thread in
  Alcotest.(check string) "Received data" "!" result

let test_poll_add_busy () =
  Eio_linux.run ~queue_depth:2 @@ fun _stdenv ->
  Switch.run @@ fun sw ->
  let r, w = Eio_linux.pipe sw in
  let a = read_one_byte ~sw r in
  let b = read_one_byte ~sw r in
  Fiber.yield ();
  let w = Option.get (Eio_linux.get_fd_opt w) |> Eio_linux.FD.to_unix `Peek in
  let sent = Unix.write w (Bytes.of_string "!!") 0 2 in
  assert (sent = 2);
  let a = Promise.await_exn a in
  Alcotest.(check string) "Received data" "!" a;
  let b = Promise.await_exn b in
  Alcotest.(check string) "Received data" "!" b

(* Write a string to a pipe and read it out again. *)
let test_copy () =
  Eio_linux.run ~queue_depth:3 @@ fun _stdenv ->
  Switch.run @@ fun sw ->
  let msg = "Hello!" in
  let from_pipe, to_pipe = Eio_linux.pipe sw in
  let buffer = Buffer.create 20 in
  Fiber.both
    (fun () -> Eio.Flow.copy from_pipe (Eio.Flow.buffer_sink buffer))
    (fun () ->
       Eio.Flow.copy (Eio.Flow.string_source msg) to_pipe;
       Eio.Flow.copy (Eio.Flow.string_source msg) to_pipe;
       Eio.Flow.close to_pipe
    );
  Alcotest.(check string) "Copy correct" (msg ^ msg) (Buffer.contents buffer);
  Eio.Flow.close from_pipe

(* Write a string via 2 pipes. The copy from the 1st to 2nd pipe will be optimised and so tests a different code-path. *)
let test_direct_copy () =
  Eio_linux.run ~queue_depth:4 @@ fun _stdenv ->
  Switch.run @@ fun sw ->
  let msg = "Hello!" in
  let from_pipe1, to_pipe1 = Eio_linux.pipe sw in
  let from_pipe2, to_pipe2 = Eio_linux.pipe sw in
  let buffer = Buffer.create 20 in
  let to_output = Eio.Flow.buffer_sink buffer in
  Switch.run (fun sw ->
      Fiber.fork ~sw (fun () -> Ctf.label "copy1"; Eio.Flow.copy from_pipe1 to_pipe2; Eio.Flow.close to_pipe2);
      Fiber.fork ~sw (fun () -> Ctf.label "copy2"; Eio.Flow.copy from_pipe2 to_output);
      Eio.Flow.copy (Eio.Flow.string_source msg) to_pipe1;
      Eio.Flow.close to_pipe1;
    );
  Alcotest.(check string) "Copy correct" msg (Buffer.contents buffer);
  Eio.Flow.close from_pipe1;
  Eio.Flow.close from_pipe2

(* Read and write using IO vectors rather than the fixed buffers. *)
let test_iovec () =
  Eio_linux.run ~queue_depth:4 @@ fun _stdenv ->
  Switch.run @@ fun sw ->
  let from_pipe, to_pipe = Eio_linux.pipe sw in
  let from_pipe = Eio_linux.get_fd from_pipe in
  let to_pipe = Eio_linux.get_fd to_pipe in
  let message = Cstruct.of_string "Got [   ] and [   ]" in
  let rec recv = function
    | [] -> ()
    | cs ->
      let got = Eio_linux.Low_level.readv from_pipe cs in
      recv (Cstruct.shiftv cs got)
  in
  Fiber.both
    (fun () -> recv [Cstruct.sub message 5 3; Cstruct.sub message 15 3])
    (fun () ->
       let b = Cstruct.of_string "barfoo" in
       Eio_linux.Low_level.writev to_pipe [Cstruct.sub b 3 3; Cstruct.sub b 0 3];
       Eio_linux.FD.close to_pipe
    );
  Alcotest.(check string) "Transfer correct" "Got [foo] and [bar]" (Cstruct.to_string message)

let () =
  let open Alcotest in
  run "eio_linux" [
    "io", [
      test_case "copy"          `Quick test_copy;
      test_case "direct_copy"   `Quick test_direct_copy;
      test_case "poll_add"      `Quick test_poll_add;
      test_case "poll_add_busy" `Quick test_poll_add_busy;
      test_case "iovec"         `Quick test_iovec;
    ];
  ]
