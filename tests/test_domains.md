# Setting up the environment

```ocaml
# #require "eunix";;
```

```ocaml
open Eio.Std

let run (fn : Eio.Domain_manager.t -> unit) =
  try
    Eunix.run @@ fun env ->
    fn (Eio.Stdenv.domain_mgr env);
    print_endline "ok"
  with
  | Failure msg -> print_endline msg
  | ex -> print_endline (Printexc.to_string ex)
```

# Test cases

Spawning a second domain:

```ocaml
# run @@ fun mgr ->
  let response = Eio.Domain_manager.run_compute_unsafe mgr (fun () -> "Hello from new domain") in
  traceln "Got %S from spawned domain" response
Got "Hello from new domain" from spawned domain
ok
- : unit = ()
```

The domain raises an exception:

```ocaml
# run @@ fun mgr ->
  Eio.Domain_manager.run_compute_unsafe mgr (fun () -> failwith "Exception from new domain")
Exception from new domain
- : unit = ()
```

We can still run other fibres in the main domain while waiting:

```ocaml
# run @@ fun mgr ->
  Switch.top @@ fun sw ->
  Fibre.both ~sw
    (fun () ->
      traceln "Spawning new domain...";
      let response = Eio.Domain_manager.run_compute_unsafe mgr (fun () -> "Hello from new domain") in
      traceln "Got %S from spawned domain" response
    )
    (fun () ->
      traceln "Other fibres can still run"
    )
Spawning new domain...
Other fibres can still run
Got "Hello from new domain" from spawned domain
ok
- : unit = ()
```
