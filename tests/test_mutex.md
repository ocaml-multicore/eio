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
Exception: Sys_error "Eio.Mutex.unlock: already unlocked!".
```

## With_lock

```ocaml
# run @@ fun () ->
  let t = M.create () in
  let fn () =
    traceln "Entered critical section";
    Fiber.yield ();
    traceln "Leaving critical section"
  in
  Fiber.both
    (fun () -> M.with_lock t ~on_exn:`Poison fn)
    (fun () -> M.with_lock t ~on_exn:`Poison fn);;
+Entered critical section
+Leaving critical section
+Entered critical section
+Leaving critical section
- : unit = ()
```

A failed critical section can poison the mutex:

```ocaml
# run @@ fun () ->
  let t = M.create () in
  try
    M.with_lock t (fun () -> failwith "Simulated error");
  with Failure _ ->
    traceln "Trying to use the failed lock again fails:";
    M.lock t;;
+Trying to use the failed lock again fails:
Exception: Eio__Eio_mutex.Poisoned (Failure "Simulated error").
```

Alternatively, you can just unlock on error:

```ocaml
# run @@ fun () ->
  let t = M.create () in
  try
    M.with_lock t ~on_exn:`Unlock (fun () -> failwith "Simulated error");
  with Failure _ ->
    traceln "Trying to use the lock again is OK:";
    M.lock t;;
+Trying to use the lock again is OK:
- : unit = ()
```

## Try_lock

```ocaml
# run @@ fun () ->
  let t = M.create () in
  let fn () =
    match M.try_lock t with
    | true ->
      traceln "Entered critical section";
      Fiber.yield ();
      traceln "Leaving critical section";
      M.unlock t
    | false ->
      traceln "Failed to get lock"
  in
  Fiber.both fn fn;
  M.with_lock t (fun () -> traceln "Lock still works");;
+Entered critical section
+Failed to get lock
+Leaving critical section
+Lock still works
- : unit = ()
```
