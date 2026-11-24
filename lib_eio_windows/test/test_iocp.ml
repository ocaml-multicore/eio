(* Copyright (C) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: ISC *)

open Eio.Std

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
  ]
