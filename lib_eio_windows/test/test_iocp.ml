(* Copyright (C) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: ISC *)

open Eio.Std

let positioned env () =
  let path = Eio.Path.(env#cwd / "iocp-positioned") in
  Eio.Path.save path "0123456789" ~create:(`Exclusive 0o600);
  Fun.protect ~finally:(fun () -> Eio.Path.unlink path) @@ fun () ->
  Eio.Path.with_open_in path @@ fun f ->
  let at = Optint.Int63.of_int in
  ignore (Eio.File.seek f (at 3) `Set);
  let buf = Cstruct.create 3 in
  Eio.File.pread_exact f ~file_offset:(at 6) [buf];
  Alcotest.(check string) "positioned data" "678" (Cstruct.to_string buf);
  Alcotest.(check int) "cursor unchanged" 3 (Optint.Int63.to_int (Eio.File.seek f (at 0) `Cur));
  let fd = Option.get (Eio_unix.Resource.fd_opt f) in
  List.iter (fun io ->
      List.iter (fun bufs ->
          match io ~file_offset:(at (-1)) fd bufs with
          | _ -> Alcotest.fail "accepted a negative file offset"
          | exception Invalid_argument _ -> ()) [ [||]; [|buf|] ]
    ) [Eio_windows.Low_level.preadv; Eio_windows.Low_level.pwritev];
  (match Eio_windows.Low_level.pwritev fd ~file_offset:(at 0) [|Cstruct.of_string "BAD"|] with
   | _ -> Alcotest.fail "write gained authority from read-only handle"
   | exception Unix.Unix_error (EBADF, _, _) -> ());
  Effect.Deep.try_with (Eio.Flow.read_exact f) buf {
    effc = fun (type a) (eff : a Effect.t) -> match eff with
      | Eio_unix.Private.Await_readable _ -> Alcotest.fail "regular file polled for readiness"
      | _ -> None
  };
  Alcotest.(check string) "sequential data" "345" (Cstruct.to_string buf)

let positioned_errors env () =
  let path = Eio.Path.(env#cwd / "iocp-positioned-public") in
  Eio.Path.with_open_out ~create:(`Exclusive 0o600) path (fun f ->
      Eio.File.pwrite_all f ~file_offset:Optint.Int63.zero [Cstruct.of_string "abc"]);
  Fun.protect ~finally:(fun () -> Eio.Path.unlink path) @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let fd = Unix.openfile "iocp-positioned-public" [O_RDONLY; O_CLOEXEC] 0 in
  let file = Eio_unix.File.import_rw ~sw ~close_unix:true fd in
  let buf = Cstruct.create 3 in
  Eio.File.pread_exact file ~file_offset:Optint.Int63.zero [buf];
  Alcotest.(check string) "positioned write and read" "abc" (Cstruct.to_string buf);
  match Eio.File.pwrite_all file ~file_offset:Optint.Int63.zero [buf] with
  | () -> Alcotest.fail "wrote to a read-only descriptor"
  | exception Eio.Io (_, _) -> ()

let sync_read env () =
  Switch.run @@ fun sw ->
  let raw, writer = Unix.pipe ~cloexec:true () in
  let r = Eio_unix.File.import_ro ~sw ~close_unix:true raw in
  (* A delayed writer also bounds the test if a read ignores cancellation. *)
  let writer = Thread.create (fun () ->
      Unix.sleepf 0.4;
      Fun.protect ~finally:(fun () -> Unix.close writer)
        (fun () -> ignore (Unix.single_write writer (Bytes.of_string "x") 0 1))) () in
  Fun.protect ~finally:(fun () -> Thread.join writer) @@ fun () ->
  let buf = Cstruct.create 1 in
  (match Eio.Time.with_timeout_exn env#clock 0.02 (fun () -> Eio.Flow.single_read r buf) with
   | _ -> Alcotest.fail "idle read completed"
   | exception Eio.Time.Timeout -> ());
  Eio.Flow.read_exact r buf;
  Alcotest.(check string) "cancelled read left data for the next reader" "x" (Cstruct.to_string buf)

let sync_pipe_alias () =
  Switch.run @@ fun sw ->
  let r, w = Unix.pipe ~cloexec:true () in
  Fun.protect ~finally:(fun () -> Unix.close w) @@ fun () ->
  let import r = Eio_unix.File.import_ro ~sw ~close_unix:true r in
  let first = import r and second = import (Unix.dup ~cloexec:true r) in
  let waits = ref 0 in
  let read flow =
    let buf = Cstruct.create 1 in
    Effect.Deep.try_with (Eio.Flow.read_exact flow) buf {
      effc = fun (type a) (eff : a Effect.t) -> match eff with
        | Eio_unix.Private.Await_readable _ -> incr waits; None
        | _ -> None
    };
    Cstruct.to_string buf
  in
  let result = Fiber.fork_promise ~sw (fun () -> read first) in
  Fiber.first (fun () -> ignore (read second)) (fun () -> ());
  ignore (Unix.single_write w (Bytes.of_string "x") 0 1);
  Alcotest.(check string) "first read" "x" (Promise.await_exn result);
  Alcotest.(check int) "only the active reader checked readiness" 1 !waits

let shared_file_cursor () =
  let path = Filename.temp_file "eio-iocp-cursor" ".tmp" in
  Fun.protect ~finally:(fun () -> Sys.remove path) @@ fun () ->
  Switch.run @@ fun sw ->
  let raw = Iocp.openfile path [Unix.O_RDWR; O_CLOEXEC] 0 in
  let import fd = Eio_unix.File.import_rw ~sw ~close_unix:true fd in
  let a = import raw and b = import (Unix.dup ~cloexec:true raw) in
  Fiber.both (fun () -> Eio.Flow.copy_string "x" a) (fun () -> Eio.Flow.copy_string "y" b);
  Alcotest.(check int) "cursor after both writes" 2 (Optint.Int63.to_int (Eio.File.seek b Optint.Int63.zero `Cur));
  ignore (Eio.File.seek a Optint.Int63.zero `Set);
  let byte f =
    let buf = Cstruct.create 1 in
    Eio.Flow.read_exact f buf;
    Cstruct.to_string buf
  in
  let x, y = Fiber.pair (fun () -> byte a) (fun () -> byte b) in
  Alcotest.(check (list string)) "one byte per alias" ["x"; "y"] (List.sort String.compare [x; y])

let close_during_readiness env () =
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.Net.socketpair_stream ~sw () in
  Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
      let ready = Fiber.fork_promise ~sw (fun () ->
          Eio_windows.Low_level.await_readable (Eio_unix.Resource.fd r)) in
      Eio.Flow.close r;
      Eio_windows.Low_level.await_writable (Eio_unix.Resource.fd w);
      Eio.Flow.copy_string "x" w;
      Promise.await_exn ready)

let tests () = List.map (fun (name, fn) -> name, `Quick, fn) [
    "close during low-level readiness wait", close_during_readiness;
    "backend identifier", (fun env () -> Alcotest.(check string) "backend" "windows" env#backend_id);
    "duplicated files share the cursor", (fun _ -> shared_file_cursor);
    "positioned file errors", positioned_errors;
    "synchronous pipe read cancellation", sync_read;
    "duplicated synchronous pipe readers", (fun _ -> sync_pipe_alias);
    "positioned IO preserves cursor and rights", positioned;
  ]
