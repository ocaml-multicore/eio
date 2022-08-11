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
        C.await_no_mutex condition;
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
        C.await_no_mutex condition;
        traceln "1: finished");
    (fun () ->
        traceln "2: wait for condition";
        C.await_no_mutex condition;
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

## Typical single-domain use

```ocaml
let x = ref 0
let cond = Eio.Condition.create ()

let set value =
  x := value;
  Eio.Condition.broadcast cond

let await p =
  (* Warning: only safe within a single-domain, and if [p] doesn't switch fibers! *)
  while not (p !x) do
    Eio.Condition.await_no_mutex cond
  done
```

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Fiber.both
    (fun () ->
       traceln "x = %d" !x;
       await ((=) 42);
       traceln "x = %d" !x
    )
    (fun () ->
       set 5;
       Fiber.yield ();
       set 7;
       set 42;
    );;
+x = 0
+x = 42
- : unit = ()
```

## Use with mutex

```ocaml
let x = ref 0
let cond = Eio.Condition.create ()
let mutex = Eio.Mutex.create ()

let set value =
  Eio.Mutex.use_rw ~protect:false mutex (fun () -> x := value);
  Eio.Condition.broadcast cond

let await p =
  Eio.Mutex.use_ro mutex (fun () ->
     while not (p !x) do
       Eio.Condition.await cond mutex
     done
  )
```

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Fiber.both
    (fun () ->
       traceln "x = %d" !x;
       await ((=) 42);
       traceln "x = %d" !x
    )
    (fun () ->
       set 5;
       Fiber.yield ();
       set 7;
       set 42;
    );;
+x = 0
+x = 42
- : unit = ()
```

Cancellation while waiting:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Fiber.first
    (fun () ->
       await ((=) 0);
       assert false;
    )
    (fun () -> ());
  Fiber.both
    (fun () ->
       traceln "x = %d" !x;
       await ((=) 0);
       traceln "x = %d" !x
    )
    (fun () ->
       set 5;
       Fiber.yield ();
       set 0;
    );;
+x = 42
+x = 0
- : unit = ()
```
