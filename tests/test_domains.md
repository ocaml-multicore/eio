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

We can still run other fibres in the main domain while waiting.
Here, we use a mutex to check that the parent domain really did run while waiting for the child domain.

```ocaml
# run @@ fun mgr ->
  let mutex = Stdlib.Mutex.create () in
  Mutex.lock mutex;
  Fibre.both
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
      traceln "Other fibres can still run";
      Mutex.unlock mutex
    );;
+Spawning new domain...
+Other fibres can still run
+Got "Hello from new domain" from spawned domain
- : unit = ()
```

Cancelling another domain:

```ocaml
# run @@ fun mgr ->
  Fibre.both
    (fun () ->
       try
         Eio.Domain_manager.run mgr (fun () ->
           try Fibre.await_cancel ()
           with ex -> traceln "Spawned domain got %a" Fmt.exn ex; raise ex
         )
       with ex -> traceln "Spawning fibre got %a" Fmt.exn ex; raise ex
    )
    (fun () -> failwith "Simulated error");;
+Spawned domain got Cancelled: Failure("Simulated error")
+Spawning fibre got Cancelled: Failure("Simulated error")
Exception: Failure "Simulated error".
```

Spawning when already cancelled - no new domain is started:

```ocaml
# run @@ fun mgr ->
  Switch.run @@ fun sw ->
  Switch.turn_off sw (Failure "Simulated error");
  Eio.Domain_manager.run mgr (fun () -> traceln "Domain spawned - shouldn't happen!");;
Exception: Failure "Simulated error".
```
