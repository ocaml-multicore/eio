# Setting up the environment

```ocaml
# #require "eio_main";;
```

```ocaml
open Eio.Std

module S = Eio.Stream

exception Cancel

let run fn =
  Eio_main.run @@ fun _ ->
  fn ()

let add t v =
  traceln "Adding %d to stream" v;
  S.add t v;
  traceln "Added %d to stream" v

let take t =
  traceln "Reading from stream";
  traceln "Got %d from stream" (S.take t)

let take_nonblocking t =
  traceln "Reading from stream";
  traceln "Got %a from stream" Fmt.(option ~none:(unit "None") int) (S.take_nonblocking t)
```

# Test cases

Simple non-blocking case

```ocaml
# run @@ fun () ->
  let t = S.create 2 in
  add t 1;
  add t 2;
  take t;
  take t;;
+Adding 1 to stream
+Added 1 to stream
+Adding 2 to stream
+Added 2 to stream
+Reading from stream
+Got 1 from stream
+Reading from stream
+Got 2 from stream
- : unit = ()
```

Readers have to wait when the stream is empty:

```ocaml
# run @@ fun () ->
  let t = S.create 2 in
  add t 1;
  Fiber.both
    (fun () -> take t; take t)
    (fun () -> add t 2);;
+Adding 1 to stream
+Added 1 to stream
+Reading from stream
+Got 1 from stream
+Reading from stream
+Adding 2 to stream
+Added 2 to stream
+Got 2 from stream
- : unit = ()
```

Writers have to wait when the stream is full:

```ocaml
# run @@ fun () ->
  let t = S.create 3 in
  add t 1;
  Fiber.both
    (fun () ->
      add t 2;
      add t 3;
      add t 4;
    )
    (fun () ->
      take t;
      take t;
      take t;
      take t
    );;
+Adding 1 to stream
+Added 1 to stream
+Adding 2 to stream
+Added 2 to stream
+Adding 3 to stream
+Added 3 to stream
+Adding 4 to stream
+Reading from stream
+Got 1 from stream
+Reading from stream
+Got 2 from stream
+Reading from stream
+Got 3 from stream
+Reading from stream
+Got 4 from stream
+Added 4 to stream
- : unit = ()
```

A zero-length queue is synchronous:

```ocaml
# run @@ fun () ->
  let t = S.create 0 in
  Fiber.both
    (fun () ->
      add t 1;
      add t 2;
    )
    (fun () ->
      take t;
      take t;
    );;
+Adding 1 to stream
+Reading from stream
+Got 1 from stream
+Reading from stream
+Added 1 to stream
+Adding 2 to stream
+Added 2 to stream
+Got 2 from stream
- : unit = ()
```

Cancel reading from a stream:

```ocaml
# run @@ fun () ->
  let t = S.create 1 in
  try
    Fiber.both
      (fun () -> take t)
      (fun () -> raise Cancel);
    assert false;
  with Cancel ->
    traceln "Cancelled";
    add t 2;
    take t;;
+Reading from stream
+Cancelled
+Adding 2 to stream
+Added 2 to stream
+Reading from stream
+Got 2 from stream
- : unit = ()
```

Cancel writing to a stream:

```ocaml
# run @@ fun () ->
  let t = S.create 1 in
  try
    Fiber.both
      (fun () -> add t 1; add t 2)
      (fun () -> raise Cancel);
    assert false;
  with Cancel ->
    traceln "Cancelled";
    take t;
    add t 3;
    take t;;
+Adding 1 to stream
+Added 1 to stream
+Adding 2 to stream
+Cancelled
+Reading from stream
+Got 1 from stream
+Adding 3 to stream
+Added 3 to stream
+Reading from stream
+Got 3 from stream
- : unit = ()
```

Cancel writing to a zero-length stream:

```ocaml
# run @@ fun () ->
  let t = S.create 0 in
  try
    Fiber.both
      (fun () -> add t 1)
      (fun () -> raise Cancel);
    assert false;
  with Cancel ->
    traceln "Cancelled";
    Fiber.both
      (fun () -> add t 2)
      (fun () -> take t);;
+Adding 1 to stream
+Cancelled
+Adding 2 to stream
+Reading from stream
+Got 2 from stream
+Added 2 to stream
- : unit = ()
```

Trying to use a stream with a cancelled context:

```ocaml
# run @@ fun () ->
  let t = S.create 0 in
  Eio.Cancel.sub @@ fun c ->
  Eio.Cancel.cancel c Cancel;
  begin try add  t 1 with ex -> traceln "%a" Fmt.exn ex end;
  begin try take t   with ex -> traceln "%a" Fmt.exn ex end;
  (* Check we released the mutex correctly: *)
  Eio.Cancel.protect @@ fun () ->
  Fiber.both
    (fun () -> add  t 1)
    (fun () -> take t)
  ;;
+Adding 1 to stream
+Cancelled: Cancel
+Reading from stream
+Cancelled: Cancel
+Adding 1 to stream
+Reading from stream
+Got 1 from stream
+Added 1 to stream
- : unit = ()
```

Readers queue up:

```ocaml
# run @@ fun () ->
  let t = S.create 0 in
  Switch.run @@ fun sw ->
  Fiber.fork ~sw (fun () -> take t; traceln "a done");
  Fiber.fork ~sw (fun () -> take t; traceln "b done");
  Fiber.fork ~sw (fun () -> take t; traceln "c done");
  add t 1;
  add t 2;
  add t 3;;
+Reading from stream
+Reading from stream
+Reading from stream
+Adding 1 to stream
+Added 1 to stream
+Adding 2 to stream
+Added 2 to stream
+Adding 3 to stream
+Added 3 to stream
+Got 1 from stream
+a done
+Got 2 from stream
+b done
+Got 3 from stream
+c done
- : unit = ()
```

Writers queue up:

```ocaml
# run @@ fun () ->
  let t = S.create 0 in
  Switch.run @@ fun sw ->
  Fiber.fork ~sw (fun () -> add t 1);
  Fiber.fork ~sw (fun () -> add t 2);
  Fiber.fork ~sw (fun () -> add t 3);
  take t;
  take t;
  take t;;
+Adding 1 to stream
+Adding 2 to stream
+Adding 3 to stream
+Reading from stream
+Got 1 from stream
+Reading from stream
+Got 2 from stream
+Reading from stream
+Got 3 from stream
+Added 1 to stream
+Added 2 to stream
+Added 3 to stream
- : unit = ()
```

Cancelling writing to a stream:

```ocaml
# run @@ fun () ->
  let t = S.create 1 in
  add t 0;
  Switch.run @@ fun sw ->
  try
    Fiber.both
      (fun () -> add t 1)
      (fun () -> raise Cancel)
  with Cancel ->
    traceln "Cancelled";
    take t;
    add t 2;
    take t;;
+Adding 0 to stream
+Added 0 to stream
+Adding 1 to stream
+Cancelled
+Reading from stream
+Got 0 from stream
+Adding 2 to stream
+Added 2 to stream
+Reading from stream
+Got 2 from stream
- : unit = ()
```

Non-blocking take:

```ocaml
# run @@ fun () ->
  let t = S.create 1 in
  take_nonblocking t;
  add t 0;
  take_nonblocking t;;
+Reading from stream
+Got None from stream
+Adding 0 to stream
+Added 0 to stream
+Reading from stream
+Got 0 from stream
- : unit = ()
```

Non-blocking take with zero-capacity stream:

```ocaml
# run @@ fun () ->
  let t = S.create 0 in
  take_nonblocking t;
  Fiber.both
    (fun () -> add t 0)
    (fun () -> take_nonblocking t);
  take_nonblocking t;;
+Reading from stream
+Got None from stream
+Adding 0 to stream
+Reading from stream
+Got 0 from stream
+Added 0 to stream
+Reading from stream
+Got None from stream
- : unit = ()
```
