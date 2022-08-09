# Setting up the environment

```ocaml
# #require "eio_main";;
# open Eio.Std;;
```

## Overriding tracing

```ocaml
# Eio_main.run @@ fun env ->
  let debug = Eio.Stdenv.debug env in
  let my_traceln = {
    Eio.Debug.traceln = fun ?__POS__:_ fmt -> Fmt.epr ("++" ^^ fmt ^^ "@.")
  } in
  Fiber.both
    (fun () ->
       Fiber.with_binding debug#traceln my_traceln @@ fun () ->
       Fiber.both
         (fun () -> traceln "a")
         (fun () -> Fiber.yield (); traceln "b")
     )
     (fun () -> traceln "c");;
++a
+c
++b
- : unit = ()
```
