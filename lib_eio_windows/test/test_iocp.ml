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
   | exception Unix.Unix_error (EBADF, _, _) -> ())

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
    "positioned file errors", positioned_errors;
    "positioned IO preserves cursor and rights", positioned;
  ]
