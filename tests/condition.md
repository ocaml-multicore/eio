# Setting up the environment

```ocaml
# #require "eio.mock";;
```

```ocaml
open Eio.Std

module C = Eio.Condition
```

# Test cases

Simple case:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Switch.run @@ fun sw ->
  let condition = C.create () in
  Fiber.both
    (fun () -> 
        traceln "1: wait for condition"; 
        C.await condition;
        traceln "1: finished")
    (fun () -> 
        traceln "2: broadcast condition";
        C.broadcast condition;
        traceln "2: finished");;
+1: wait for condition
+2: broadcast condition
+2: finished
+1: finished
- : unit = ()
```

Broadcast when no one is waiting doesn't block:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Switch.run @@ fun sw ->
  let condition = C.create () in
  traceln "broadcast condition";
  C.broadcast condition;
  traceln "finished";;
+broadcast condition
+finished
- : unit = ()
```

Broadcast wakes all waiters at once:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Switch.run @@ fun sw ->
  let condition = C.create () in
  Fiber.all [
    (fun () -> 
        traceln "1: wait for condition"; 
        C.await condition;
        traceln "1: finished");
    (fun () -> 
        traceln "2: wait for condition"; 
        C.await condition;
        traceln "2: finished");
    (fun () -> 
        traceln "3: broadcast condition";
        C.broadcast condition;
        traceln "3: finished")
  ];;
+1: wait for condition
+2: wait for condition
+3: broadcast condition
+3: finished
+1: finished
+2: finished
- : unit = ()
```
