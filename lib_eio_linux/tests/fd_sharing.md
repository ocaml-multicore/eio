# Setting up the environment

```ocaml
# #require "eio_linux";;
```

```ocaml
open Eio.Std
```

# Tests

One domain closes an FD after another domain has enqueued a uring operation mentioning it.

```ocaml
# Eio_linux.run @@ fun env ->
  let dm = env#domain_mgr in
  Switch.run @@ fun sw ->
  let m = Mutex.create () in
  Mutex.lock m;
  let r, w = Eio_unix.pipe sw in
  let ready, set_ready = Promise.create () in
  Fiber.both
    (fun () ->
       Eio.Domain_manager.run dm (fun () ->
         Fiber.both
           (fun () ->
              traceln "Domain 1 enqueuing read on FD";
              let buf = Cstruct.create 1 in
              match Eio.Flow.single_read r buf with
              | _ -> assert false
              | exception End_of_file -> traceln "Read EOF"
           )
           (fun () ->
               (* We have enqueued a read request, but not yet submitted it to Linux.
                  Wait for [m] to prevent submission until the main domain is ready. *)
               traceln "Waiting for domain 0...";
               Promise.resolve set_ready ();
               Mutex.lock m;
               traceln "Domain 1 flushing queue"
           )
        )
    )
    (fun () ->
       Promise.await ready;
       traceln "Domain 0 closing FD";
       Eio.Flow.close r;
       Fiber.yield ();
       traceln "Domain 0 closed FD; waking domain 1";
       Mutex.unlock m;
       (* Allow the read to complete. *)
       Eio.Flow.close w
    );;
+Domain 1 enqueuing read on FD
+Waiting for domain 0...
+Domain 0 closing FD
+Domain 0 closed FD; waking domain 1
+Domain 1 flushing queue
+Read EOF
- : unit = ()
```
