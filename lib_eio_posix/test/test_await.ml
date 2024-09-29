open Eio.Std

let () =
  Eio_posix.run @@ fun _ ->
  let a, b = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
  (* Start awaiting readable/writable state, but cancel immediately. *)
  try
    Eio.Cancel.sub (fun cc ->
        Fiber.all [
          (fun () -> Eio_unix.await_readable a);
          (fun () -> Eio_unix.await_writable b);
          (fun () -> Eio.Cancel.cancel cc Exit);
        ];
        assert false
      )
  with Eio.Cancel.Cancelled _ ->
    (* Now wait for something else. Will fail if the old FDs are still being waited on. *)
    let c, d = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
    Unix.close a;
    Unix.close b;
    Fiber.first
      (fun () -> Eio_unix.await_readable c)
      (fun () -> Eio_unix.await_writable d);
    Unix.close c;
    Unix.close d
