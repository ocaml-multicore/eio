# Set up the test environment

```ocaml
# #require "eio_luv";;
# open Eio.Std;;
# open Eio;;
```

A helper function to create two sockets and pass their FDs to a function:

```ocaml
let with_sockets fn =
  Eio_luv.run @@ fun _env ->
  Switch.run @@ fun sw ->
  let src, dst = Eio_unix.socketpair ~sw () in
  let src_fd = Option.get @@ Eio_unix.FD.peek_opt src in
  let dst_fd = Option.get @@ Eio_unix.FD.peek_opt dst in
  fn ~sw ((src, src_fd), (dst, dst_fd))
```

Waiting for the same file descriptor to become writable does not raise `EEXIST`.

```ocaml
# with_sockets @@ fun ~sw ((src, src_fd), (dst, dst_fd)) ->
  Eio.Fiber.both
    (fun () -> Eio_unix.await_writable src_fd)
    (fun () -> Eio_unix.await_writable src_fd);;
- : unit = ()
```

An example of reading and writing with different file descriptors.

```ocaml
# with_sockets @@ fun ~sw ((src, src_fd), (dst, dst_fd)) ->
  let message = "hello" in
  let buffer = Buffer.create (String.length message) in
  Eio.Fiber.both
    (fun () -> 
      Eio_unix.await_readable src_fd; 
      Eio.Flow.copy src (Flow.buffer_sink buffer))
    (fun () -> 
      Eio_unix.await_writable dst_fd; 
      Eio.Flow.copy_string message dst; 
      Eio.Flow.close dst
    );
  Buffer.contents buffer;;
- : string = "hello"
```

Waiting for reading and writing on the same file descriptor.

```ocaml
# with_sockets @@ fun ~sw ((src, src_fd), (dst, dst_fd)) ->
  let message = "hello" in
  let buffer = Buffer.create (String.length message) in
  Eio.Fiber.both
    (fun () -> 
      Eio_unix.await_writable src_fd;
      Eio_unix.await_readable src_fd; 
      Eio.Flow.copy src (Flow.buffer_sink buffer))
    (fun () -> 
      Eio_unix.await_writable dst_fd; 
      Eio.Flow.copy_string message dst;
      Eio.Flow.close dst
    );
  Buffer.contents buffer;;
- : string = "hello"
```

Cancelling a fiber removes a fiber but does not stop polling if others are still waiting.

```ocaml
# with_sockets @@ fun ~sw ((src, src_fd), (dst, _dst_fd)) ->
  let buffer = Buffer.create 5 in
  Fiber.fork ~sw (fun () -> 
    Eio_unix.await_readable src_fd; 
    Eio.Flow.copy src (Flow.buffer_sink buffer);
    traceln "Still received: %s" (Buffer.contents buffer)
  );
  (try 
    Eio.Fiber.both
      (fun () -> Eio_unix.await_readable src_fd)
      (fun () -> raise (Failure "Simulate a cancel"))
  with
    exn -> traceln "%s" (Printexc.to_string exn));
  Flow.copy_string "Hello" dst;
  Flow.close dst;;
+Failure("Simulate a cancel")
+Still received: Hello
- : unit = ()
```

