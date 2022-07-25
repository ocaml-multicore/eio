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

let mock_source items =
  object
    inherit Eio.Flow.source

    val mutable items = items

    method read_methods = []

    method read_into buf =
      match items with
      | [] -> raise End_of_file
      | x :: xs ->
        let len = min (Cstruct.length buf) (Cstruct.length x) in
        Cstruct.blit x 0 buf 0 len;
        items <- Cstruct.shiftv (x :: xs) len;
        len
  end
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

Copying from src using a plain buffer (the default):

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
Copying from src using `Read_source_buffer`:

```ocaml
# run @@ fun () ->
  let src = Eio.Flow.string_source "foobar" in
  let dst = Eio_mock.Flow.make "dst" in
  Eio_mock.Flow.set_copy_method dst `Read_source_buffer;
  Eio_mock.Flow.on_copy_bytes dst [`Return 3; `Return 5];
  Eio.Flow.copy src dst;;
+dst: wrote (rsb) ["foo"]
+dst: wrote (rsb) ["bar"]
- : unit = ()
```
