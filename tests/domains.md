# Setting up the environment

```ocaml
# #require "eio_main";;
```

```ocaml
open Eio.Std

let run (fn : Eio.Domain_manager.ty r -> unit) =
  Eio_main.run @@ fun env ->
  fn (Eio.Stdenv.domain_mgr env)
```

# Test cases

Spawning a second domain:

```ocaml
# run @@ fun mgr ->
  let response = Eio.Domain_manager.run mgr (fun () -> "Hello from new domain") in
  traceln "Got %S from spawned domain" response;;
+Got "Hello from new domain" from spawned domain
- : unit = ()
```

The domain raises an exception:

```ocaml
# run @@ fun mgr ->
  Eio.Domain_manager.run mgr (fun () -> failwith "Exception from new domain");;
Exception: Failure "Exception from new domain".
```

We can still run other fibers in the main domain while waiting.
Here, we use a mutex to check that the parent domain really did run while waiting for the child domain.

```ocaml
# run @@ fun mgr ->
  let mutex = Stdlib.Mutex.create () in
  Mutex.lock mutex;
  Fiber.both
    (fun () ->
      traceln "Spawning new domain...";
      let response = Eio.Domain_manager.run mgr (fun () ->
        Mutex.lock mutex;
        Mutex.unlock mutex;
        "Hello from new domain"
        ) in
      traceln "Got %S from spawned domain" response
    )
    (fun () ->
      traceln "Other fibers can still run";
      Mutex.unlock mutex
    );;
+Spawning new domain...
+Other fibers can still run
+Got "Hello from new domain" from spawned domain
- : unit = ()
```

Cancelling another domain:

```ocaml
# run @@ fun mgr ->
  Fiber.both
    (fun () ->
       try
         Eio.Domain_manager.run mgr (fun () ->
           try Fiber.await_cancel ()
           with ex -> traceln "Spawned domain got %a" Fmt.exn ex; raise ex
         )
       with ex -> traceln "Spawning fiber got %a" Fmt.exn ex; raise ex
    )
    (fun () -> failwith "Simulated error");;
+Spawned domain got Cancelled: Failure("Simulated error")
+Spawning fiber got Cancelled: Failure("Simulated error")
Exception: Failure "Simulated error".
```

Spawning when already cancelled - no new domain is started:

```ocaml
# run @@ fun mgr ->
  Switch.run @@ fun sw ->
  Switch.fail sw (Failure "Simulated error");
  Eio.Domain_manager.run mgr (fun () -> traceln "Domain spawned - shouldn't happen!");;
Exception: Failure "Simulated error".
```

Using a cancellation context across domains is not permitted:

```ocaml
# run @@ fun mgr ->
  Switch.run @@ fun sw ->
  let p, r = Promise.create () in
  Fiber.both
    (fun () ->
       Eio.Domain_manager.run mgr @@ fun () ->
       Eio.Cancel.sub @@ fun cc ->
       Promise.resolve r cc;
       Fiber.await_cancel ()
    )
    (fun () ->
       let cc = Promise.await p in
       Eio.Cancel.cancel cc Exit
    );;
Exception:
Invalid_argument "Cancellation context accessed from wrong domain!".
```

Likewise, switches can't be shared:

```ocaml
# run @@ fun mgr ->
  Switch.run @@ fun sw ->
  let p, r = Promise.create () in
  Fiber.both
    (fun () ->
       Eio.Domain_manager.run mgr @@ fun () ->
       Switch.run @@ fun sw ->
       Promise.resolve r sw;
       Fiber.await_cancel ()
    )
    (fun () ->
       let sw = Promise.await p in
       Switch.fail sw Exit
    );;
Exception: Invalid_argument "Switch accessed from wrong domain!".
```

Registering a release handler across domains:

```ocaml
# run @@ fun mgr ->
  Switch.run @@ fun sw ->
  Eio.Domain_manager.run mgr (fun () ->
     Switch.on_release sw (fun () -> traceln "Handler called");
     traceln "Handler registered in new domain";
  );
  traceln "Sub-domain finished; ending switch"
+Handler registered in new domain
+Sub-domain finished; ending switch
+Handler called
- : unit = ()
```

Cancelling a release handler across domains:

```ocaml
# run @@ fun mgr ->
  Switch.run @@ fun sw ->
  let p, r = Promise.create () in
  Fiber.both
    (fun () ->
       Eio.Domain_manager.run mgr @@ fun () ->
       Switch.run @@ fun sw ->
       let hook = Switch.on_release_cancellable sw (fun () -> traceln "BUG") in
       Promise.resolve r hook;
       Fiber.await_cancel ()
    )
    (fun () ->
       let hook = Promise.await p in
       let cancelled = Switch.try_remove_hook hook in
       traceln "Cancelled: %b" cancelled;
       raise Exit
    );;
+Cancelled: true
Exception: Stdlib.Exit.
```

Can't fork into another domain:

```ocaml
# run @@ fun mgr ->
  Switch.run @@ fun sw ->
  let p, r = Promise.create () in
  Fiber.both
    (fun () ->
       Eio.Domain_manager.run mgr @@ fun () ->
       Switch.run @@ fun sw ->
       Promise.resolve r sw;
       Fiber.await_cancel ()
    )
    (fun () ->
       let sw = Promise.await p in
       Fiber.fork ~sw ignore;
    );;
Exception: Invalid_argument "Switch accessed from wrong domain!".
```

# Fiber-local storage

Fiber-local bindings are not propagated when spawning fibers in other
domains (as the values may not be thread-safe):

```ocaml
# run @@ fun mgr ->
  let key = Fiber.create_key () in
  Fiber.with_binding key 123 @@ fun () ->
  Eio.Domain_manager.run mgr @@ fun () ->
  traceln "Key => %a" Fmt.(option ~none:(const string "<unset>") int) (Fiber.get key);;
+Key => <unset>
- : unit = ()
```
