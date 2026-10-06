let () =
  Eio_posix.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let t0 = Eio.Time.now clock in
  Eio.Fiber.first
    (fun () -> Eio.Condition.await_no_mutex Eio_unix.Process.sigchld)
    (fun () -> Eio.Time.sleep clock 5.0);
  assert (Eio.Time.now clock -. t0 < 5.0)
