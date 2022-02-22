# Set up the test environment

```ocaml
# #require "eio_luv";;
# open Eio.Std;;
```

```ocaml
let rec read_exactly fd buf =
  let size = Luv.Buffer.size buf in
  if size > 0 then (
    let got = Eio_luv.Low_level.File.read fd [buf] |> Eio_luv.Low_level.or_raise |> Unsigned.Size_t.to_int in
    let next = Luv.Buffer.sub buf ~offset:got ~length:(size - got) in
    read_exactly fd next
  )

let () =
  Luv.Error.set_on_unhandled_exception @@ fun ex ->
  Printf.printf "Unhandled luv exception: %s\n%!" (Printexc.to_string ex)
```

# Hello, world

```ocaml
# Eio_luv.run @@ fun env ->
  Eio.Flow.copy_string "Hello, world!\n" (Eio.Stdenv.stdout env);;
Hello, world!
- : unit = ()
```

# Read a few bytes from /dev/zero

```ocaml
let main _stdenv =
  Switch.run @@ fun sw ->
  let fd = Eio_luv.Low_level.File.open_ ~sw "/dev/zero" [] |> Eio_luv.Low_level.or_raise in
  let buf = Luv.Buffer.create 4 in
  read_exactly fd buf;
  traceln "Read %S" (Luv.Buffer.to_string buf);
  Eio_luv.Low_level.File.close fd
```

```ocaml
# Eio_luv.run main;;
+Read "\000\000\000\000"
- : unit = ()
```

# Test cancellation

```ocaml
let main env =
  let name = "hang.pipe" in
  Unix.mkfifo name 0o700;
  Fun.protect ~finally:(fun () -> Unix.unlink name) @@ fun () ->
  Switch.run @@ fun sw ->
  let fd = Eio_luv.Low_level.File.open_ ~sw name [`NONBLOCK] |> Eio_luv.Low_level.or_raise in
  Fiber.both
    (fun () -> read_exactly fd (Luv.Buffer.create 1))
    (fun () -> raise Exit);;
```

```ocaml
# Eio_luv.run main;;
Exception: Stdlib.Exit.
```
