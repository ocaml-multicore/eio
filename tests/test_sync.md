# Setting up the environment

```ocaml
# #require "eio_main";;
```

```ocaml
open Eio.Std

module Ctf = Eio.Private.Ctf

let pp_promise pp f x =
  match Promise.peek x with
  | None -> Fmt.string f "unresolved"
  | Some Error (Failure msg) -> Fmt.pf f "broken:%s" msg
  | Some Error ex -> Fmt.pf f "broken:%a" Fmt.exn ex
  | Some Ok x -> Fmt.pf f "fulfilled:%a" pp x
```

# Test cases

Create a promise, fork a thread waiting for it, then fulfull it:
```ocaml
# let () =
    Eio_main.run @@ fun _stdenv ->
    Switch.run @@ fun sw ->
    let p, r = Promise.create () in
    traceln "Initial state: %a" (pp_promise Fmt.string) p;
    let thread = Fiber.fork_promise ~sw (fun () -> Promise.await_exn p) in
    Promise.resolve_ok r "ok";
    traceln "After being fulfilled: %a" (pp_promise Fmt.string) p;
    traceln "Thread before yield: %a" (pp_promise Fmt.string) thread;
    Fiber.yield ();
    traceln "Thread after yield: %a" (pp_promise Fmt.string) thread;
    traceln "Final result: %s" (Promise.await_exn thread);;
+Initial state: unresolved
+After being fulfilled: fulfilled:ok
+Thread before yield: unresolved
+Thread after yield: fulfilled:ok
+Final result: ok
```

Create a promise, fork a thread waiting for it, then break it:
```ocaml
# let () =
    Eio_main.run @@ fun _stdenv ->
    Switch.run @@ fun sw ->
    let p, r = Promise.create () in
    traceln "Initial state: %a" (pp_promise Fmt.string) p;
    let thread = Fiber.fork_promise ~sw (fun () -> Promise.await_exn p) in
    Promise.resolve_error r (Failure "test");
    traceln "After being broken: %a" (pp_promise Fmt.string) p;
    traceln "Thread before yield: %a" (pp_promise Fmt.string) thread;
    Fiber.yield ();
    traceln "Thread after yield: %a" (pp_promise Fmt.string) thread;
    match Promise.await_exn thread with
    | x -> failwith x
    | exception (Failure msg) -> traceln "Final result exception: %s" msg;;
+Initial state: unresolved
+After being broken: broken:test
+Thread before yield: unresolved
+Thread after yield: broken:test
+Final result exception: test
```

Some simple tests of `fork`:
```ocaml
# let () =
    Eio_main.run @@ fun _stdenv ->
    let i = ref 0 in
    Switch.run (fun sw ->
        Fiber.fork ~sw (fun () -> incr i);
      );
    traceln "Forked code ran; i is now %d" !i;
    let p1, r1 = Promise.create () in
    try
      Switch.run (fun sw ->
          Fiber.fork ~sw (fun () -> Promise.await p1; incr i; raise Exit);
          traceln "Forked code waiting; i is still %d" !i;
          Promise.resolve r1 ()
        );
      assert false
    with Exit ->
      traceln "Forked code ran; i is now %d" !i;;
+Forked code ran; i is now 1
+Forked code waiting; i is still 1
+Forked code ran; i is now 2
```

Basic semaphore tests:
```ocaml
# let () =
    let module Semaphore = Eio.Semaphore in
    Eio_main.run @@ fun _stdenv ->
    Switch.run @@ fun sw ->
    let running = ref 0 in
    let sem = Semaphore.make 2 in
    let fork = Fiber.fork_promise ~sw in
    let a = fork (fun () -> Ctf.label "a"; Semaphore.acquire sem; incr running) in
    let b = fork (fun () -> Ctf.label "b"; Semaphore.acquire sem; incr running) in
    let c = fork (fun () -> Ctf.label "c"; Semaphore.acquire sem; incr running) in
    let d = fork (fun () -> Ctf.label "d"; Semaphore.acquire sem; incr running) in
    traceln "Semaphore means that only %d threads are running" !running;
    Promise.await_exn a;
    Promise.await_exn b;
    (* a finishes and c starts *)
    decr running;
    Semaphore.release sem;
    traceln "One finished; now %d is running " !running;
    Fiber.yield ();
    traceln "Yield allows C to start; now %d are running " !running;
    Promise.await_exn c;
    (* b finishes and d starts *)
    decr running;
    Semaphore.release sem;
    Promise.await_exn d;
    decr running;
    Semaphore.release sem;
    decr running;
    Semaphore.release sem;;
+Semaphore means that only 2 threads are running
+One finished; now 1 is running
+Yield allows C to start; now 2 are running
```

Releasing a semaphore when no-one is waiting for it:
```ocaml
# let () =
    let module Semaphore = Eio.Semaphore in
    Eio_main.run @@ fun _stdenv ->
    Switch.run @@ fun sw ->
    let sem = Semaphore.make 0 in
    Semaphore.release sem;        (* Release with free-counter *)
    traceln "Initial config: %d" (Semaphore.get_value sem);
    Fiber.fork ~sw (fun () -> Ctf.label "a"; Semaphore.acquire sem);
    Fiber.fork ~sw (fun () -> Ctf.label "b"; Semaphore.acquire sem);
    traceln "A running: %d" (Semaphore.get_value sem);
    Semaphore.release sem;        (* Release with a non-empty wait-queue *)
    traceln "Now b running: %d" (Semaphore.get_value sem);
    Semaphore.release sem;        (* Release with an empty wait-queue *)
    traceln "Finished: %d" (Semaphore.get_value sem);;
+Initial config: 1
+A running: 0
+Now b running: 0
+Finished: 1
```
