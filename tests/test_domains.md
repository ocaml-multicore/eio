# Setting up the environment

```ocaml
# #require "eio_main";;
```

```ocaml
open Eio.Std

let run (fn : Eio.Domain_manager.t -> unit) =
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

Can't register a release handler across domains:

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
       Switch.on_release sw ignore
    );;
Exception: Invalid_argument "Switch accessed from wrong domain!".
```

Can't release a release handler across domains:

```ocaml
# run @@ fun mgr ->
  Switch.run @@ fun sw ->
  let p, r = Promise.create () in
  Fiber.both
    (fun () ->
       Eio.Domain_manager.run mgr @@ fun () ->
       Switch.run @@ fun sw ->
       let hook = Switch.on_release_cancellable sw ignore in
       Promise.resolve r hook;
       Fiber.await_cancel ()
    )
    (fun () ->
       let hook = Promise.await p in
       Switch.remove_hook hook
    );;
Exception: Invalid_argument "Switch hook removed from wrong domain!".
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

Creating a context key:

```ocaml
# let key : int Eio.Context.key = Eio.Context.create_key ();;
val key : int Eio.Context.key = <abstr>
# let trace_key () =
  let value = Eio.Context.get key in
  traceln "Key => %a" Fmt.(option ~none:(const string "<unset>") int) value;;
val trace_key : unit -> unit = <fun>
```

Keys default to being unset

```ocaml
# run @@ fun _ ->
  trace_key ();;
+Key => <unset>
- : unit = ()
```

`with_value` can be used to define a key.

```ocaml
# run @@ fun _ ->
  Eio.Context.with_value key 123 @@ fun () -> trace_key ();;
+Key => 123
- : unit = ()
```

`with_value` will shadow variables defined in outer scopes.

```ocaml
# run @@ fun _ ->
  Eio.Context.with_value key 123 @@ fun () ->
  trace_key ();
  Eio.Context.with_value key 456 (fun () -> trace_key ());
  trace_key ();;
+Key => 123
+Key => 456
+Key => 123
- : unit = ()
```

Values are propagated when forking, or sending fibers to other domains.

```ocaml
# run @@ fun _ ->
  Eio.Context.with_value key 123 @@ fun () ->
  Switch.run @@ fun sw ->
  Fiber.fork ~sw trace_key;;
+Key => 123
- : unit = ()
# run @@ fun mgr->
  Eio.Context.with_value key 123 @@ fun () ->
  Eio.Domain_manager.run mgr @@ fun () ->
  trace_key ();;
+Key => 123
- : unit = ()
```

Values are inherited from the currently running fiber, rather than the switch.

```ocaml
# run @@ fun _ ->
  Switch.run @@ fun sw ->
  Eio.Context.with_value key 123 @@ fun () ->
  Fiber.fork ~sw trace_key;;
+Key => 123
- : unit = ()
```
