```ocaml
# #require "eio_js_backend";;
# open Eio.Std;;
# Eio_js_backend.start @@ fun () ->
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
