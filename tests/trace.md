```ocaml
# #require "eio_main";;
# open Eio.Std;;
# Eio_main.run @@ fun _env ->
  traceln "One-line trace";
  traceln "@[<v2>A nested list@,Foo@,Bar@]";
  traceln "Trace with position" ~__POS__:("trace.md", 5, 1, 10);;
+One-line trace
+A nested list
+  Foo
+  Bar
+Trace with position [trace.md:5]
- : unit = ()
```
