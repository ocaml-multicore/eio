# Setting up the environment

```ocaml
# #require "eio_main";;
```

```ocaml
open Eio.Std

module C = Eio.Condition

let run fn =
  Eio_main.run @@ fun _ ->
  fn ()
```

# Test cases


Simple case:

```ocaml
# run @@ fun () ->
  Switch.run @@ fun sw ->
  let condition = C.create () in
  Fiber.all
    [
    (fun () -> 
        traceln "1: wait for condition"; 
        C.wait condition;
        traceln "1: finished");
    (fun () -> 
        traceln "2: signal condition";
        C.signal condition ();
        traceln "2: finished")
    ];;
+1: wait for condition
+2: signal condition
+2: finished
+1: finished
- : unit = ()
```

Signal wakes a single waiter:

```ocaml
# run @@ fun () ->
  Switch.run @@ fun sw ->
  let condition = C.create () in
  Fiber.all
    [
    (fun () -> 
        traceln "1: wait for condition"; 
        C.wait condition;
        traceln "1: finished");
    (fun () -> 
        traceln "2: wait for condition"; 
        C.wait condition;
        traceln "2: finished");
    (fun () -> 
        traceln "3: signal first condition";
        C.signal condition ();
        Eio.Fiber.yield ();
        traceln "3: signal second condition";
        C.signal condition ();
        traceln "3: finished")
    ];;
+1: wait for condition
+2: wait for condition
+3: signal first condition
+1: finished
+3: signal second condition
+3: finished
+2: finished
- : unit = ()
```

Broadcast wakes all waiters at once:

```ocaml
# run @@ fun () ->
  Switch.run @@ fun sw ->
  let condition = C.create () in
  Fiber.all
    [
    (fun () -> 
        traceln "1: wait for condition"; 
        C.wait condition;
        traceln "1: finished");
    (fun () -> 
        traceln "2: wait for condition"; 
        C.wait condition;
        traceln "2: finished");
    (fun () -> 
        traceln "3: broadcast condition";
        C.broadcast condition ();
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
