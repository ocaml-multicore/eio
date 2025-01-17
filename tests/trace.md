```ocaml
# #require "eio_main";;
# open Eio.Std;;
# Eio_main.run @@ fun _env ->
  traceln "One-line trace";
+One-line trace
- : unit = ()
```
