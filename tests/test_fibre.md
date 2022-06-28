# Setting up the environment

```ocaml
# #require "eio_main";;
```

```ocaml
open Eio.Std

let run fn =
  Eio_main.run @@ fun _ ->
  traceln "%s" (fn ())
```

# Fiber.first

First finishes, second is cancelled:

```ocaml
# run @@ fun () ->
  let p, r = Promise.create () in
  Fiber.first
    (fun () -> "a")
    (fun () -> Promise.await p);;
+a
- : unit = ()
```

Second finishes, first is cancelled:

```ocaml
# run @@ fun () ->
  let p, r = Promise.create () in
  Fiber.first
    (fun () -> Promise.await p)
    (fun () -> "b");;
+b
- : unit = ()
```

If both succeed, we pick the first one:

```ocaml
# run @@ fun () ->
  Fiber.first
    (fun () -> "a")
    (fun () -> "b");;
+a
- : unit = ()
```

One crashes - report it:

```ocaml
# run @@ fun () ->
  Fiber.first
    (fun () -> "a")
    (fun () -> failwith "b crashed");;
Exception: Failure "b crashed".
```

```ocaml
# run @@ fun () ->
  Fiber.first
    (fun () -> failwith "a crashed")
    (fun () -> "b");;
Exception: Failure "a crashed".
```

Both crash - report both:

```ocaml
# run @@ fun () ->
  Fiber.first
    (fun () -> failwith "a crashed")
    (fun () -> failwith "b crashed");;
Exception: Multiple exceptions:
Failure("a crashed")
and
Failure("b crashed")
```

Cancelled before it can crash:

```ocaml
# run @@ fun () ->
  Fiber.first
    (fun () -> "a")
    (fun () -> Fiber.yield (); failwith "b crashed");;
+a
- : unit = ()
```

One claims to be cancelled (for some reason other than the other fiber finishing):

```ocaml
# run @@ fun () ->
  Fiber.first
    (fun () -> raise (Eio.Cancel.Cancelled (Failure "cancel-a")))
    (fun () -> "b");;
Exception: Cancelled: Failure("cancel-a")
```

```ocaml
# run @@ fun () ->
  Fiber.first
    (fun () -> Fiber.yield (); "a")
    (fun () -> raise (Eio.Cancel.Cancelled (Failure "cancel-b")));;
Exception: Cancelled: Failure("cancel-b")
```

Cancelled from parent:

```ocaml
# run @@ fun () ->
  let p, r = Promise.create () in
  Fiber.both
    (fun () ->
      failwith @@ Fiber.first
        (fun () -> Promise.await p)
        (fun () -> Promise.await p)
    )
    (fun () -> failwith "Parent cancel");
  "not-reached";;
Exception: Failure "Parent cancel".
```

Cancelled from parent while already cancelling:

```ocaml
# run @@ fun () ->
  Fiber.both
    (fun () ->
      let _ = Fiber.first
        (fun () -> "a")
        (fun () -> Fiber.yield (); failwith "cancel-b")
      in
      traceln "Parent cancel failed"
    )
    (fun () -> traceln "Cancelling parent"; failwith "Parent cancel");
  "not-reached";;
+Cancelling parent
Exception: Failure "Parent cancel".
```

Cancelling in a sub-switch. We see the exception as `Cancelled Exit` when we're being asked to cancel,
but just as plain `Exit` after we leave the context in which the cancellation started:

```ocaml
# run @@ fun () ->
  let p, r = Promise.create () in
  Fiber.both
    (fun () ->
      try
        Switch.run (fun _ ->
          try Promise.await p
          with ex -> traceln "Nested exception: %a" Fmt.exn ex; raise ex
        )
      with ex -> traceln "Parent exception: %a" Fmt.exn ex; raise ex
    )
    (fun () -> raise Exit);
  failwith "not-reached";;
+Nested exception: Cancelled: Stdlib.Exit
+Parent exception: Cancelled: Stdlib.Exit
Exception: Stdlib.Exit.
```

# Fiber.pair

```ocaml
# run @@ fun () ->
  let x, y = Fiber.pair (fun () -> "a") (fun () -> "b") in
  x ^ y;;
+ab
- : unit = ()
```

# Fiber.all

```ocaml
# run @@ fun () ->
  Fiber.all [];
  Fiber.all (List.init 3 (fun x () -> traceln "fiber %d" x));
  "done";;
+fiber 0
+fiber 1
+fiber 2
+done
- : unit = ()
```

# Fiber.any

```ocaml
# run @@ fun () ->
  string_of_int @@
  Fiber.any (List.init 3 (fun x () -> traceln "%d" x; Fiber.yield (); x));;
+0
+1
+2
+0
- : unit = ()
```

# Fiber.await_cancel

```ocaml
# run @@ fun () ->
  Fiber.both
    (fun () ->
       try Fiber.await_cancel ()
       with Eio.Cancel.Cancelled _ as ex ->
         traceln "Caught: %a" Fmt.exn ex;
         raise ex
    )
    (fun () -> failwith "simulated error");
  "not reached";;
+Caught: Cancelled: Failure("simulated error")
Exception: Failure "simulated error".
```

# Fiber.fork

`Fiber.fork_promise ~sw` inherits the cancellation context from `sw`, not from the current fiber:

```ocaml
# run @@ fun () ->
  let switch = ref None in
  Fiber.both
    (fun () ->
       Switch.run @@ fun sw ->
       switch := Some sw;
       Fiber.await_cancel ()
    )
    (fun () ->
      let sw = Option.get !switch in
      Eio.Cancel.protect @@ fun () ->
      let child = Fiber.fork_promise ~sw (fun () ->
         traceln "Forked child";
         Fiber.await_cancel ()
      ) in
      Switch.fail sw Exit;
      Promise.await_exn child
    );
  "not reached";;
+Forked child
Exception: Stdlib.Exit.
```

# Scheduling order

Forking runs the child first, and puts the calling fiber at the head of the run-queue.

```ocaml
# run @@ fun () ->
  Switch.run @@ fun sw ->
  Fiber.fork ~sw (fun () -> traceln "1st child runs"; Fiber.yield (); traceln "Queued work");
  Fiber.fork ~sw (fun () -> traceln "2nd child runs immediately");
  traceln "Caller runs before queued work";
  "ok";;
+1st child runs
+2nd child runs immediately
+Caller runs before queued work
+Queued work
+ok
- : unit = ()
```

Same with `both`:

```ocaml
# run @@ fun () ->
  Switch.run @@ fun sw ->
  Fiber.fork ~sw (fun () -> traceln "Enqueuing work for later"; Fiber.yield (); traceln "Queued work");
  Fiber.both
    (fun () -> traceln "1st branch")
    (fun () -> traceln "2nd branch");
  "ok";;
+Enqueuing work for later
+1st branch
+2nd branch
+Queued work
+ok
- : unit = ()
```

Same with `first`:

```ocaml
# run @@ fun () ->
  Switch.run @@ fun sw ->
  Fiber.fork ~sw (fun () -> traceln "Enqueuing work for later"; Fiber.yield (); traceln "Queued work");
  Fiber.first
    (fun () -> traceln "1st branch")
    (fun () -> traceln "2nd branch");
  "ok";;
+Enqueuing work for later
+1st branch
+2nd branch
+Queued work
+ok
- : unit = ()
```

# Forking while cancelled

```ocaml
# run @@ fun () ->
  Fiber.first
    (fun () -> failwith "Simulated error")
    (fun () ->
       Fiber.both
         (fun () -> traceln "Not reached")
         (fun () -> traceln "Not reached");
       assert false
    );;
Exception: Failure "Simulated error".
```
