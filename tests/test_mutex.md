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
  begin
    try M.unlock t
    with Sys_error msg -> traceln "Caught: %s" msg
  end;
  traceln "Trying to use lock after error...";
  M.lock t;;
+Caught: Eio.Mutex.unlock: already unlocked!
+Trying to use lock after error...
Exception:
Eio__Eio_mutex.Poisoned (Sys_error "Eio.Mutex.unlock: already unlocked!").
```

## Read-write access

Successful use; only one critical section is active at once:

```ocaml
# run @@ fun () ->
  let t = M.create () in
  let fn () =
    traceln "Entered critical section";
    Fiber.yield ();
    traceln "Leaving critical section"
  in
  Fiber.both
    (fun () -> M.use_rw ~protect:true t fn)
    (fun () -> M.use_rw ~protect:true t fn);;
+Entered critical section
+Leaving critical section
+Entered critical section
+Leaving critical section
- : unit = ()
```

A failed critical section will poison the mutex:

```ocaml
# run @@ fun () ->
  let t = M.create () in
  try
    M.use_rw ~protect:true t (fun () -> failwith "Simulated error");
  with Failure _ ->
    traceln "Trying to use the failed lock again fails:";
    M.lock t;;
+Trying to use the failed lock again fails:
Exception: Eio__Eio_mutex.Poisoned (Failure "Simulated error").
```

## Protection

We can prevent cancellation during a critical section:

```ocaml
# run @@ fun () ->
  let t = M.create () in
  Fiber.both
    (fun () ->
       M.use_rw ~protect:true t (fun () -> Fiber.yield (); traceln "Restored invariant");
       Fiber.check ();
       traceln "Error: not cancelled!";
    )
    (fun () -> traceln "Cancelling..."; failwith "Simulated error");;
+Cancelling...
+Restored invariant
Exception: Failure "Simulated error".
```

Or allow interruption and disable the mutex:

```ocaml
# run @@ fun () ->
  let t = M.create () in
  try
    Fiber.both
      (fun () ->
         M.use_rw ~protect:false t (fun () -> Fiber.yield (); traceln "Restored invariant")
      )
      (fun () -> traceln "Cancelling..."; failwith "Simulated error");
   with ex ->
     traceln "Trying to reuse the failed mutex...";
     M.use_ro t (fun () -> assert false);;
+Cancelling...
+Trying to reuse the failed mutex...
Exception:
Eio__Eio_mutex.Poisoned (Eio__Exn.Cancelled (Failure "Simulated error")).
```

Protection doesn't prevent cancellation while we're still waiting for the lock, though:

```ocaml
# run @@ fun () ->
  let t = M.create () in
  M.lock t;
  try
    Fiber.both
      (fun () -> M.use_rw ~protect:true t (fun () -> assert false))
      (fun () -> traceln "Cancelling..."; failwith "Simulated error")
  with Failure _ ->
    M.unlock t;
    M.use_ro t (fun () -> traceln "Can reuse the mutex");;
+Cancelling...
+Can reuse the mutex
- : unit = ()
```

Poisoning wakes any wakers:

```ocaml
# run @@ fun () ->
  let t = M.create () in
  Fiber.both
    (fun () ->
       try
         M.use_rw ~protect:false t (fun () ->
            Fiber.yield ();
            traceln "Poisoning mutex";
            failwith "Simulated error"
         )
       with Failure _ -> ()
    )
    (fun () -> traceln "Waiting for lock..."; M.use_ro t (fun () -> assert false));;
+Waiting for lock...
+Poisoning mutex
Exception: Eio__Eio_mutex.Poisoned (Failure "Simulated error").
```


## Read-only access

If the resource isn't being mutated, we can just unlock on error:

```ocaml
# run @@ fun () ->
  let t = M.create () in
  try
    M.use_ro t (fun () -> failwith "Simulated error");
  with Failure msg ->
    traceln "Caught: %s" msg;
    traceln "Trying to use the lock again is OK:";
    M.lock t;;
+Caught: Simulated error
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
  M.use_ro t (fun () -> traceln "Lock still works");;
+Entered critical section
+Failed to get lock
+Leaving critical section
+Lock still works
- : unit = ()
```
