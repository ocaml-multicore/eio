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

# Fibre.first

First finishes, second is cancelled:

```ocaml
# run @@ fun () ->
  let p, r = Promise.create () in
  Fibre.first
    (fun () -> "a")
    (fun () -> Promise.await p);;
+a
- : unit = ()
```

Second finishes, first is cancelled:

```ocaml
# run @@ fun () ->
  let p, r = Promise.create () in
  Fibre.first
    (fun () -> Promise.await p)
    (fun () -> "b");;
+b
- : unit = ()
```

If both succeed, we pick the first one:

```ocaml
# run @@ fun () ->
  Fibre.first
    (fun () -> "a")
    (fun () -> "b");;
+a
- : unit = ()
```

One crashes - report it:

```ocaml
# run @@ fun () ->
  Fibre.first
    (fun () -> "a")
    (fun () -> failwith "b crashed");;
Exception: Failure "b crashed".
```

```ocaml
# run @@ fun () ->
  Fibre.first
    (fun () -> failwith "a crashed")
    (fun () -> "b");;
Exception: Failure "a crashed".
```

Both crash - report both:

```ocaml
# run @@ fun () ->
  Fibre.first
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
  Fibre.first
    (fun () -> "a")
    (fun () -> Fibre.yield (); failwith "b crashed");;
+a
- : unit = ()
```

One claims to be cancelled (for some reason other than the other fibre finishing):

```ocaml
# run @@ fun () ->
  Fibre.first
    (fun () -> raise (Eio.Cancel.Cancelled (Failure "cancel-a")))
    (fun () -> "b");;
Exception: Cancelled: Failure("cancel-a")
```

```ocaml
# run @@ fun () ->
  Fibre.first
    (fun () -> Fibre.yield (); "a")
    (fun () -> raise (Eio.Cancel.Cancelled (Failure "cancel-b")));;
Exception: Cancelled: Failure("cancel-b")
```

Cancelled from parent:

```ocaml
# run @@ fun () ->
  let p, r = Promise.create () in
  Fibre.both
    (fun () ->
      failwith @@ Fibre.first
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
  Fibre.both
    (fun () ->
      let _ = Fibre.first
        (fun () -> "a")
        (fun () -> Fibre.yield (); failwith "cancel-b")
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
  Fibre.both
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

# Fibre.pair

```ocaml
# run @@ fun () ->
  let x, y = Fibre.pair (fun () -> "a") (fun () -> "b") in
  x ^ y;;
+ab
- : unit = ()
```

# Fibre.all

```ocaml
# run @@ fun () ->
  Fibre.all [];
  Fibre.all (List.init 3 (fun x () -> traceln "fibre %d" x));
  "done";;
+fibre 0
+fibre 1
+fibre 2
+done
- : unit = ()
```

# Fibre.any

```ocaml
# run @@ fun () ->
  string_of_int @@
  Fibre.any (List.init 3 (fun x () -> traceln "%d" x; Fibre.yield (); x));;
+0
+1
+2
+0
- : unit = ()
```

# Fibre.await_cancel

```ocaml
# run @@ fun () ->
  Fibre.both
    (fun () ->
       try Fibre.await_cancel ()
       with Eio.Cancel.Cancelled _ as ex ->
         traceln "Caught: %a" Fmt.exn ex;
         raise ex
    )
    (fun () -> failwith "simulated error");
  "not reached";;
+Caught: Cancelled: Failure("simulated error")
Exception: Failure "simulated error".
```

# Fibre.fork

`Fibre.fork_promise ~sw` inherits the cancellation context from `sw`, not from the current fibre:

```ocaml
# run @@ fun () ->
  let switch = ref None in
  Fibre.both
    (fun () ->
       Switch.run @@ fun sw ->
       switch := Some sw;
       Fibre.await_cancel ()
    )
    (fun () ->
      let sw = Option.get !switch in
      Eio.Cancel.protect @@ fun () ->
      let child = Fibre.fork_promise ~sw (fun () ->
         traceln "Forked child";
         Fibre.await_cancel ()
      ) in
      Switch.turn_off sw Exit;
      Promise.await child
    );
  "not reached";;
+Forked child
Exception: Stdlib.Exit.
```

# Scheduling order

Forking runs the child first, and puts the calling fibre at the head of the run-queue.

```ocaml
# run @@ fun () ->
  Switch.run @@ fun sw ->
  Fibre.fork ~sw (fun () -> traceln "1st child runs"; Fibre.yield (); traceln "Queued work");
  Fibre.fork ~sw (fun () -> traceln "2nd child runs immediately");
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
  Fibre.fork ~sw (fun () -> traceln "Enqueuing work for later"; Fibre.yield (); traceln "Queued work");
  Fibre.both
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
  Fibre.fork ~sw (fun () -> traceln "Enqueuing work for later"; Fibre.yield (); traceln "Queued work");
  Fibre.first
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
