# Setting up the environment

```ocaml
# #require "eio_main";;
```

```ocaml
open Eio.Std

module M = Eio.Mutex

let run fn =
  Eio_main.run @@ fun _ ->
  fn ()

let lock t =
  traceln "Locking";
  M.lock t;
  traceln "Locked"

let unlock t =
  traceln "Unlocking";
  M.unlock t;
  traceln "Unlocked"
```

# Test cases

Simple case

```ocaml
# run @@ fun () ->
  let t = M.create () in
  lock t;
  unlock t;
  lock t;
  unlock t;;
+Locking
+Locked
+Unlocking
+Unlocked
+Locking
+Locked
+Unlocking
+Unlocked
- : unit = ()
```

Concurrent access to the mutex


```ocaml
# run @@ fun () ->
  let t = M.create () in
  let fn () = 
    lock t;
    Eio.Fiber.yield ();
    unlock t
  in
  List.init 4 (fun _ -> fn)
  |> Fiber.all;;
+Locking
+Locked
+Locking
+Locking
+Locking
+Unlocking
+Unlocked
+Locked
+Unlocking
+Unlocked
+Locked
+Unlocking
+Unlocked
+Locked
+Unlocking
+Unlocked
- : unit = ()
```

Double unlock raises an exception

```ocaml
# run @@ fun () ->
  let t = M.create () in
  M.lock t;
  M.unlock t;
  M.unlock t;;
Exception: Eio__Eio_mutex.Already_unlocked.
```
