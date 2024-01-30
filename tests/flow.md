## Setting up the environment

```ocaml
# #require "eio_main";;
# #require "eio.mock";;
```

```ocaml
open Eio.Std

let run fn =
  Eio_main.run @@ fun _ ->
  fn ()

let mock_source =
  let module X = struct
    type t = Cstruct.t list ref

    let read_methods = []

    let single_read t buf =
      match !t with
      | [] -> raise End_of_file
      | x :: xs ->
        let len = min (Cstruct.length buf) (Cstruct.length x) in
        Cstruct.blit x 0 buf 0 len;
        t := Cstruct.shiftv (x :: xs) len;
        len
  end in
  let ops = Eio.Flow.Pi.source (module X) in
  fun items -> Eio.Resource.T (ref items, ops)
```

## read_exact

```ocaml
# run @@ fun () ->
  let data = List.map Cstruct.of_string ["foo"; "bar"] in
  let test n =
    let buf = Cstruct.create n in
    Eio.Flow.read_exact (mock_source data) buf;
    traceln "Got %S" (Cstruct.to_string buf)
  in
  test 0;
  test 3;
  test 5;
  test 6;
  test 7;;
+Got ""
+Got "foo"
+Got "fooba"
+Got "foobar"
Exception: End_of_file.
```

## copy

```ocaml
# run @@ fun () ->
  let src = Eio_mock.Flow.make "src" in
  let dst = Eio_mock.Flow.make "dst" in
  Eio_mock.Flow.on_read src [`Return "foo"; `Return "bar"];
  Eio.Flow.copy src dst;;
+src: read "foo"
+dst: wrote "foo"
+src: read "bar"
+dst: wrote "bar"
- : unit = ()
```

Copying from a string src:

```ocaml
# run @@ fun () ->
  let src = Eio.Flow.string_source "foobar" in
  let dst = Eio_mock.Flow.make "dst" in
  Eio_mock.Flow.on_copy_bytes dst [`Return 3; `Return 5];
  Eio.Flow.copy src dst;;
+dst: wrote "foo"
+dst: wrote "bar"
- : unit = ()
```

Copying from src using a plain buffer (the default):

```ocaml
# run @@ fun () ->
  let src = Eio.Flow.cstruct_source [Cstruct.of_string "foobar"] in
  let dst = Eio_mock.Flow.make "dst" in
  Eio_mock.Flow.on_copy_bytes dst [`Return 3; `Return 5];
  Eio.Flow.copy src dst;;
+dst: wrote "foo"
+dst: wrote "bar"
- : unit = ()
```

Copying from src using `Read_source_buffer`:

```ocaml
# run @@ fun () ->
  let src = Eio.Flow.cstruct_source [Cstruct.of_string "foobar"] in
  let dst = Eio_mock.Flow.make "dst" in
  Eio_mock.Flow.set_copy_method dst `Read_source_buffer;
  Eio_mock.Flow.on_copy_bytes dst [`Return 3; `Return 5];
  Eio.Flow.copy src dst;;
+dst: wrote (rsb) ["foo"]
+dst: wrote (rsb) ["bar"]
- : unit = ()
```

## read_all

```ocaml
# run @@ fun () ->
  let each = String.init 256 Char.chr in
  let data = List.init 40 (fun _ -> Cstruct.of_string each) in
  let got = Eio.Flow.read_all (mock_source data) in
  traceln "Input length: %d\nOutput length: %d\nEqual: %b"
    (Cstruct.lenv data) (String.length got) (String.equal got (Cstruct.copyv data));
  ;;
+Input length: 10240
+Output length: 10240
+Equal: true
- : unit = ()
```

## write

```ocaml
# run @@ fun () ->
  let dst = Eio_mock.Flow.make "dst" in
  Eio_mock.Flow.on_copy_bytes dst [`Return 6];
  Eio.Flow.write dst [Cstruct.of_string "foobar"];;
+dst: wrote "foobar"
- : unit = ()
```

## Pipes

Writing to and reading from a pipe.

```ocaml
# Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  let msg = "Hello, world" in
  Eio.Fiber.both
    (fun () ->
      let buf = Cstruct.create (String.length msg) in
      let () = Eio.Flow.read_exact r buf in
      traceln "Got: %s" (Cstruct.to_string buf)
    )
    (fun () ->
      Eio.Flow.copy_string msg w;
      Eio.Flow.close w
    );;
+Got: Hello, world
- : unit = ()
```

Make sure we don't crash on SIGPIPE:

```ocaml
# Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  Eio.Flow.close r;
  try
    Eio.Flow.copy_string "Test" w;
    assert false
  with Eio.Io (Eio.Net.E Connection_reset _, _) ->
    traceln "Connection_reset (good)";;
+Connection_reset (good)
- : unit = ()
```

## IO_MAX

Sending a very long vector over a flow should just send it in chunks, not fail:

```ocaml
# Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  let a = Cstruct.of_string "abc" in
  let vecs = List.init 10_000 (Fun.const a) in
  Fiber.both
    (fun () ->
       Eio.Flow.write w vecs;
       Eio.Flow.close w
    )
    (fun () ->
       let got = Eio.Flow.read_all r in
       traceln "Read %d bytes" (String.length got);
       assert (got = Cstruct.to_string (Cstruct.concat vecs))
    )
+Read 30000 bytes
- : unit = ()
```

## Starvation

Even if a fiber is already ready to run, we still perform IO from time to time:

```ocaml
# run @@ fun _ ->
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  let rec spin () = Fiber.yield (); spin () in
  Fiber.fork_daemon ~sw spin;
  Fiber.both
    (fun () ->
       let buf = Cstruct.create 3 in
       Eio.Flow.read_exact r buf;
       traceln "Got %S" (Cstruct.to_string buf)
    )
    (fun () ->
       Eio.Flow.write w [Cstruct.of_string "msg"]
    )
+Got "msg"
- : unit = ()
```
