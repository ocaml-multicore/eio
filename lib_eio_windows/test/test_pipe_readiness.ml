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

let tests () = [
  "independent pipe and cancelled busy read", `Quick, busy;
]
