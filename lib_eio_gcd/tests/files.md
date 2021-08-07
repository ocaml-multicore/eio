# Set up the test environment

```ocaml
# #require "eio_gcd";;
# open Eio.Std;;
```

# Hello, world

```ocaml
# Eio_gcd.run @@ fun env ->
  Eio.Flow.copy_string "Hello, world!\n" (Eio.Stdenv.stdout env)
Hello, world!
- : unit = ()
```

# Pipe

Eventually `eio_gcd` will not include any Unix specific dependencies allowing it (in theory)
to run on iOS without any trouble, but for now it works:

```ocaml
# Eio_gcd.run @@ fun env ->
  Switch.top @@ fun sw ->
  let msg = "Hello!" in
  let from_pipe, to_pipe = Eio_gcd.pipe () in
  let buffer = Buffer.create 20 in
  Fibre.both ~sw
    (fun () -> Eio.Flow.copy from_pipe (Eio.Flow.buffer_sink buffer))
    (fun () ->
       Eio.Flow.copy (Eio.Flow.string_source msg) to_pipe;
       Eio.Flow.copy (Eio.Flow.string_source msg) to_pipe;
       Eio.Flow.close to_pipe
    );
  Eio.Flow.copy_string (Buffer.contents buffer) (Eio.Stdenv.stdout env);
  Eio.Flow.close from_pipe
Hello!Hello!
- : unit = ()
```
