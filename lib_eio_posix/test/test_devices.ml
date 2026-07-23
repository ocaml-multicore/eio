(* Regression test for https://github.com/ocaml-multicore/eio/issues/757.
   macOS poll rejects /dev/null/tty with POLLNVAL. *)

open Eio.Std

let with_blocking_fd path flags fn =
  Switch.run @@ fun sw ->
  let fd = Unix.openfile path flags 0 in
  fn (Eio_unix.Fd.of_unix ~sw ~blocking:true ~close_unix:true fd)

let () =
  Eio_posix.run @@ fun _env ->
  with_blocking_fd "/dev/null" [Unix.O_WRONLY] (fun dev_null ->
    let msg = Bytes.of_string "regression test for #757\n" in
    let wrote = Eio_posix.Low_level.write dev_null msg 0 (Bytes.length msg) in
    assert (wrote = Bytes.length msg));
  with_blocking_fd "/dev/zero" [Unix.O_RDONLY] (fun dev_zero ->
    let buf = Bytes.make 4 'x' in
    let got = Eio_posix.Low_level.read dev_zero buf 0 4 in
    assert (got = 4 && Bytes.to_string buf = "\x00\x00\x00\x00"));
  begin match Unix.openfile "/dev/tty" [Unix.O_RDWR] 0 with
   | exception Unix.Unix_error (code, _, _) ->
     traceln "Skipping /dev/tty test (%s)" (Unix.error_message code)
   | fd ->
     Switch.run @@ fun sw ->
     let tty = Eio_unix.Fd.of_unix ~sw ~blocking:true ~close_unix:true fd in
     Eio_posix.Low_level.await_writable "test" tty;
     (* Check the read can be cancelled *)
     let cancelled =
       Fiber.first
         (fun () ->
            let buf = Bytes.create 1 in
            ignore (Eio_posix.Low_level.read tty buf 0 1 : int);
            false)
         (fun () -> Eio_unix.sleep 0.2; true)
     in
     assert cancelled
  end;
  traceln "test_devices: ok"
