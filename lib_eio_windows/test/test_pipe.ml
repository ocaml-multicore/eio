(* Tests for anonymous pipes *)

open Eio.Std

let read_all flow =
  let b = Buffer.create 16 in
  Eio.Flow.copy flow (Eio.Flow.buffer_sink b);
  Buffer.contents b

let test_transfer () =
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  Eio.Flow.copy_string "hello" w;
  Eio.Flow.close w;
  Alcotest.(check string) "transfer" "hello" (read_all r)

let test_read_before_write () =
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  Fiber.both
    (fun () ->
       let buf = Cstruct.create 8 in
       let n = Eio.Flow.single_read r buf in
       Alcotest.(check string) "data" "ping" (Cstruct.to_string ~len:n buf))
    (fun () -> Eio.Flow.copy_string "ping" w)

let test_eof () =
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  Eio.Flow.close w;
  let buf = Cstruct.create 1 in
  match Eio.Flow.single_read r buf with
  | _ -> Alcotest.fail "read should have signaled eof"
  | exception End_of_file -> ()

let tests = [
  "transfer", `Quick, test_transfer;
  "read-before-write", `Quick, test_read_before_write;
  "eof", `Quick, test_eof;
]
