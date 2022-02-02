# Setting up the environment

```ocaml
# #require "eio_main";;
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

# read_exact

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
