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
  Eio_unix.Fd.use_exn "write" writer (fun fd -> write_message port fd "");
  let check_readable () = Eio_unix.await_readable r in
  if read_async then (
    let registration = Iocp.register port r in
    Fun.protect ~finally:(fun () -> Iocp.close_handle registration) check_readable;
    Alcotest.(check bool) "readiness left the port empty" true (Iocp.wait port ~timeout:0 = None)
  ) else check_readable ();
  Eio_unix.Fd.use_exn "write" writer (fun fd ->
      List.iter (write_message port fd) ["hello"; ""]);
  Eio_unix.Fd.close writer;
  Alcotest.(check string) "empty message followed by a short read" "he" (read 2);
  Alcotest.(check string) "remaining bytes" "llo" (read 8);
  Alcotest.check_raises "empty message followed by close" End_of_file (fun () -> ignore (read 1))

let read_stdin ~socket () =
  Eio_windows.run @@ fun env ->
  Eio.Time.with_timeout_exn env#clock 5.0 @@ fun () ->
  let buf = Cstruct.create 2 in
  List.iter (fun expected ->
      if socket then Eio_unix.await_readable Unix.stdin;
      Eio.Flow.read_exact env#stdin buf;
      if Cstruct.to_string buf <> expected then failwith "unexpected stdin contents")
    ["bc"; "de"];
  if socket then Eio_unix.await_readable Unix.stdin;
  match Eio.Flow.single_read env#stdin buf with
  | exception End_of_file -> ()
  | _ -> failwith "expected stdin EOF"

let run_child ?(socket=false) fd =
  let exe = Unix.realpath Sys.executable_name in
  let arg = if socket then "--socket-stdin-child" else "--pipe-stdin-child" in
  let pid = Unix.create_process exe [|exe; arg|] fd
      (Unix.descr_of_out_channel stdout) (Unix.descr_of_out_channel stderr) in
  Alcotest.(check bool) "child read stdin" true (snd (Unix.waitpid [] pid) = Unix.WEXITED 0)

let inherited_stdin _env () =
  let path = Filename.temp_file "eio-stdin" ".txt" in
  Fun.protect ~finally:(fun () -> Sys.remove path) @@ fun () ->
  Out_channel.with_open_bin path (fun oc -> output_string oc "abcde");
  let check fd =
    ignore (Unix.lseek fd 1 Unix.SEEK_SET);
    run_child fd;
    Alcotest.(check int) "inherited file position" 5 (Unix.lseek fd 0 Unix.SEEK_CUR)
  in
  let sync = Unix.openfile path [Unix.O_RDONLY] 0 in
  Fun.protect ~finally:(fun () -> Unix.close sync) (fun () -> check sync);
  let fd = Iocp.openfile path [Unix.O_RDONLY] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  let port = Iocp.create () in
  Fun.protect ~finally:(fun () -> Iocp.close port) @@ fun () ->
  let handle = Iocp.register port fd in
  Fun.protect ~finally:(fun () -> Iocp.close_handle handle) @@ fun () ->
  check fd;
  Alcotest.(check bool) "no completion on parent's port" true (Iocp.wait port ~timeout:0 = None)

let inherited_socket _env () =
  let r, w = Unix.socketpair ~cloexec:true PF_UNIX SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close r; Unix.close w) @@ fun () ->
  Alcotest.(check int) "socket data" 4 (Unix.send_substring w "bcde" 0 4 []);
  Unix.shutdown w Unix.SHUTDOWN_SEND;
  run_child ~socket:true r

let tests () = [
  "synchronous message pipe", `Quick, messages ~read_async:false;
  "overlapped message pipe", `Quick, messages ~read_async:true;
  "inherited overlapped stdin", `Quick, inherited_stdin;
  "inherited socket stdin", `Quick, inherited_socket;
]
