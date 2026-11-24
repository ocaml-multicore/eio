(* Copyright (C) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: ISC *)

open Eio.Std

let write_message port fd msg =
  let req = Iocp.Event.write port ~data:() fd (Cstruct.of_string msg) ~offset:0L in
  let c = match Iocp.result req with
    | Some c -> c
    | None ->
      match Iocp.wait port ~timeout:5000 with
      | Some (Iocp.Completion ((), c)) -> c
      | _ -> Alcotest.fail "pipe write did not complete"
  in
  Alcotest.(check bool) "write succeeded" true (c.error = None);
  Alcotest.(check int) "message size" (String.length msg) c.bytes_transferred

let messages ~read_async env () =
  Switch.run @@ fun sw ->
  let r, w = Iocp.pipe ~read_async ~message:true () in
  let source = Eio_unix.File.import_ro ~sw ~close_unix:true r in
  let writer = Eio_unix.Fd.of_unix ~sw ~blocking:false ~close_unix:true w in
  let port = Iocp.create ~skip_on_success:true () in
  Fun.protect ~finally:(fun () -> Iocp.close port) @@ fun () ->
  Eio.Time.with_timeout_exn env#clock 5.0 @@ fun () ->
  let read n =
    let buf = Cstruct.create n in
    let received = Eio.Flow.single_read source buf in
    Cstruct.to_string (Cstruct.sub buf 0 received)
  in
  Fiber.first (fun () -> ignore (read 2); Alcotest.fail "empty pipe was readable") Fiber.yield;
  Eio_unix.Fd.use_exn "write" writer (fun fd ->
      List.iter (write_message port fd) [""; "hello"; ""]);
  Eio_unix.Fd.close writer;
  Alcotest.(check string) "empty message followed by a short read" "he" (read 2);
  Alcotest.(check string) "remaining bytes" "llo" (read 8);
  Alcotest.check_raises "empty message followed by close" End_of_file (fun () -> ignore (read 1))

let tests () = [
  "synchronous message pipe", `Quick, messages ~read_async:false;
  "overlapped message pipe", `Quick, messages ~read_async:true;
]
