# Set up the test environment

```ocaml
# #require "eio_luv";;
# open Eio.Std;;
```

```ocaml
let rec read_exactly ~sw fd buf =
  let size = Luv.Buffer.size buf in
  if size > 0 then (
    let got = Eio_luv.File.read ~sw fd [buf] |> Eio_luv.or_raise |> Unsigned.Size_t.to_int in
    let next = Luv.Buffer.sub buf ~offset:got ~length:(size - got) in
    read_exactly ~sw fd next
  )

let () =
  Luv.Error.set_on_unhandled_exception @@ fun ex ->
  Printf.printf "Unhandled luv exception: %s\n%!" (Printexc.to_string ex)
```

# Hello, world

```ocaml
# Eio_luv.run @@ fun env ->
  Eio.Flow.copy_string "Hello, world!\n" (Eio.Stdenv.stdout env)
Hello, world!
- : unit = ()
```

# Read a few bytes from /dev/zero

```ocaml
let main _stdenv =
  Switch.top @@ fun sw ->
  let fd = Eio_luv.File.open_ ~sw "/dev/zero" [] |> Eio_luv.or_raise in
  let buf = Luv.Buffer.create 4 in
  read_exactly ~sw fd buf;
  traceln "Read %S" (Luv.Buffer.to_string buf);
  Eio_luv.File.close fd
```

```ocaml
# Eio_luv.run main
+Read "\000\000\000\000"
- : unit = ()
```
