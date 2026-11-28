(* Copyright (C) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: ISC *)

open Eio.Std

let with_pipe f =
  let r, w = Unix.pipe ~cloexec:true () in
  Fun.protect ~finally:(fun () -> Unix.close r; Unix.close w) (fun () -> f r w)

let write_byte fd = ignore (Unix.single_write_substring fd "x" 0 1)

let busy env () =
  with_pipe @@ fun r w ->
  with_pipe @@ fun other peer ->
  let progress, started = Eio.Promise.create () in
  let reader = Thread.create (fun () ->
      Eio.Promise.resolve started ();
      ignore (Unix.read r (Bytes.create 1) 0 1)) () in
  Promise.await progress;
  Eio.Time.sleep env#clock 0.03;
  let finished = Atomic.make false and forced = Atomic.make false in
  let watchdog = Thread.create (fun () ->
      let rec wait n =
        if n > 0 && not (Atomic.get finished) then (Thread.delay 0.01; wait (n - 1))
      in
      wait 200;
      Atomic.set forced (not (Atomic.get finished));
      write_byte w) () in
  Fun.protect ~finally:(fun () -> Atomic.set finished true; Thread.join watchdog; Thread.join reader) @@ fun () ->
  Switch.run @@ fun sw ->
  let wait () = Eio.Time.with_timeout env#clock 0.1 (fun () -> Eio_unix.await_readable r; Ok ()) in
  let local = Fiber.fork_promise ~sw wait in
  let remote = Fiber.fork_promise ~sw (fun () -> Eio.Domain_manager.run env#domain_mgr wait) in
  Eio.Time.sleep env#clock 0.02;
  write_byte peer;
  Eio_unix.await_readable other;
  Alcotest.(check bool) "local wait cancelled" true (Promise.await_exn local = Error `Timeout);
  Alcotest.(check bool) "remote domain exited" true (Promise.await_exn remote = Error `Timeout);
  Alcotest.(check bool) "watchdog still waiting" false (Atomic.get forced)

external pipe_write_size : Unix.file_descr -> int -> int = "caml_eio_windows_pipe_write_size"

let capacity env () =
  with_pipe @@ fun r w ->
  let n = pipe_write_size w max_int in
  let chunk = Bytes.make n 'x' in
  Eio.Time.with_timeout_exn env#clock 3.0 @@ fun () ->
  Eio_unix.await_writable w;
  Alcotest.(check int) "fill anonymous pipe" n (Unix.single_write w chunk 0 n);
  let unsupported () =
    match Eio_unix.await_writable w with
    | () -> Alcotest.fail "zero write quota reported as writable"
    | exception Unix.Unix_error (EOPNOTSUPP, _, _) -> ()
  in
  unsupported ();
  Alcotest.(check int) "drain pipe" n (Unix.read r chunk 0 n);
  Eio_unix.await_writable w;
  write_byte w;
  Alcotest.(check int) "write after drain" 1 (Unix.read r chunk 0 1)

let pending_reader env () =
  with_pipe @@ fun r w ->
  let n = pipe_write_size w max_int in
  let started, signal = Promise.create () in
  let reader = Thread.create (fun () ->
      Promise.resolve signal ();
      ignore (Unix.read r (Bytes.create n) 0 n)) () in
  Fun.protect ~finally:(fun () -> write_byte w; Thread.join reader) @@ fun () ->
  Promise.await started;
  Eio.Time.with_timeout_exn env#clock 3.0 @@ fun () ->
  let rec wait () =
    match Eio_unix.await_writable w with
    | () -> Fiber.yield (); wait ()
    | exception Unix.Unix_error (EOPNOTSUPP, _, _) -> ()
  in
  wait ()

let tests () = [
  "independent pipe and cancelled busy read", `Quick, busy;
  "write capacity", `Quick, capacity;
  "quota reserved by a pending reader", `Quick, pending_reader;
]
