# Setting up the environment

```ocaml
# #require "eio.mock";;
```

```ocaml
open Eio.Std

let run fn =
  Eio_mock.Backend.run @@ fun _ ->
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

# Concurrent list operations

```ocaml
let process fn x =
  traceln "Start %d" x;
  Fiber.yield ();
  let y = fn x in
  traceln "Finished %d" x;
  y

let is_even x = (x land 1 = 0)

let string_even x =
  if is_even x then Some (string_of_int x)
  else None

let crash_on_three x =
  if x = 3 then failwith "Simulated error"
  else string_even x
```

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Fiber.List.filter (process is_even) [1; 2; 3; 4]
  |> traceln "%a" Fmt.(Dump.list int);;
+Start 1
+Start 2
+Start 3
+Start 4
+Finished 1
+Finished 2
+Finished 3
+Finished 4
+[2; 4]
- : unit = ()
```

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Fiber.List.map (process string_even) [1; 2; 3; 4]
  |> traceln "%a" Fmt.Dump.(list (option string));;
+Start 1
+Start 2
+Start 3
+Start 4
+Finished 1
+Finished 2
+Finished 3
+Finished 4
+[None; Some "2"; None; Some "4"]
- : unit = ()
```

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Fiber.List.filter_map (process string_even) [1; 2; 3; 4]
  |> traceln "%a" Fmt.Dump.(list string);;
+Start 1
+Start 2
+Start 3
+Start 4
+Finished 1
+Finished 2
+Finished 3
+Finished 4
+["2"; "4"]
- : unit = ()
```

If any fiber raises, everything is cancelled:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Fiber.List.filter_map (process crash_on_three) [1; 2; 3; 4]
  |> traceln "%a" Fmt.Dump.(list string);;
+Start 1
+Start 2
+Start 3
+Start 4
+Finished 1
+Finished 2
Exception: Failure "Simulated error".
```

The number of concurrent fibers can be limited:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let ps = Array.init 4 (fun _ -> Promise.create ()) in
  let await i = Promise.await (fst ps.(i)) in
  let finish i = Promise.resolve (snd (ps.(i))) in
  Fiber.both
    (fun () ->
       Fiber.List.map ~max_fibers:2 (process await) (List.init 4 Fun.id)
       |> traceln "%a" Fmt.(Dump.list string)
    )
    (fun () ->
       finish 1 "one";
       Fiber.yield ();
       finish 2 "two";
       Fiber.yield (); Fiber.yield ();
       finish 0 "zero";
       Fiber.yield (); Fiber.yield ();
       finish 3 "three";
    );;
+Start 0
+Start 1
+Finished 1
+Start 2
+Finished 2
+Start 3
+Finished 0
+Finished 3
+[zero; one; two; three]
- : unit = ()
```

Handling exceptions while waiting for a free fiber:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let ps = Array.init 2 (fun _ -> Promise.create ()) in
  let await i = Promise.await_exn (fst ps.(i)) in
  let finish i = Promise.resolve (snd (ps.(i))) in
  Fiber.both
    (fun () ->
       Fiber.List.map ~max_fibers:1 (process await) (List.init 2 Fun.id)
       |> traceln "%a" Fmt.(Dump.list string)
    )
    (fun () ->
       Fiber.yield ();
       finish 0 (Error (Failure "Simulated error"))
    );;
+Start 0
Exception: Failure "Simulated error".
```

Simple iteration:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let ps = Array.init 4 (fun _ -> Promise.create ()) in
  let await i = Promise.await (fst ps.(i)) in
  let finish i = Promise.resolve (snd (ps.(i))) () in
  Fiber.both
    (fun () ->
       Fiber.List.iter ~max_fibers:2 (process await) (List.init 4 Fun.id)
    )
    (fun () ->
       finish 1;
       Fiber.yield ();
       finish 2;
       Fiber.yield (); Fiber.yield ();
       finish 0;
       Fiber.yield (); Fiber.yield ();
       finish 3;
    );;
+Start 0
+Start 1
+Finished 1
+Start 2
+Finished 2
+Start 3
+Finished 0
+Finished 3
- : unit = ()
```

# Daemon fibers

A daemon fiber runs until the non-daemon threads finish:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Switch.run @@ fun sw ->
  Fiber.fork_daemon ~sw (fun () ->
    for i = 1 to 10 do
      traceln "Daemon running";
      Fiber.yield ()
    done;
    failwith "Test failed"
  );
  traceln "Main running 1";
  Fiber.yield ();
  traceln "Main running 2";;
+Daemon running
+Main running 1
+Daemon running
+Main running 2
- : unit = ()
```

A more complex example with multiple daemon and non-daemon fibers:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Switch.run @@ fun sw ->
  Fiber.fork ~sw (fun () ->
    traceln "Worker 1 starting";
    Fiber.yield ();
    traceln "Worker 1 running";
    Fiber.yield ();
    traceln "Worker 1 finished"
  );
  Fiber.fork ~sw (fun () ->
    traceln "Worker 2 starting";
    Fiber.yield ();
    traceln "Worker 2 finished"
  );
  Fiber.fork_daemon ~sw (fun () ->
    try
      for i = 1 to 10 do
        traceln "Daemon 1 running";
        Fiber.yield ()
      done;
      failwith "Test failed"
    with Eio.Cancel.Cancelled _ as ex ->
      traceln "Daemon cancelled; trying to spawn more fibers";
      Fiber.fork_daemon ~sw (fun () -> failwith "Shouldn't start");
      Fiber.fork ~sw (fun () -> failwith "Shouldn't start");
      raise ex
  );
  Fiber.fork_daemon ~sw (fun () ->
    traceln "Daemon 2 running";
    Fiber.yield ();
    traceln "Daemon 2 finished";
    `Stop_daemon
  );
  traceln "Main running";
  Fiber.yield ();
  traceln "Main finished";;
+Worker 1 starting
+Worker 2 starting
+Daemon 1 running
+Daemon 2 running
+Main running
+Worker 1 running
+Worker 2 finished
+Daemon 1 running
+Daemon 2 finished
+Main finished
+Worker 1 finished
+Daemon cancelled; trying to spawn more fibers
- : unit = ()
```

Failing daemon fibers still get their errors reported:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Switch.run @@ fun sw ->
  Fiber.fork_daemon ~sw (fun () ->
     Fiber.yield ();
     failwith "Simulated error"
  );
  Fiber.yield ();;
Exception: Failure "Simulated error".
```

# Fiber-local storage

Creating a context key:

```ocaml
# let key : int Fiber.key = Fiber.create_key ();;
val key : int Fiber.key = <abstr>

# let trace_key () =
  let value = Fiber.get key in
  traceln "Key => %a" Fmt.(option ~none:(const string "<unset>") int) value;;
val trace_key : unit -> unit = <fun>
```

Keys default to being unset

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  trace_key ();;
+Key => <unset>
- : unit = ()
```

`with_binding` can be used to define a key.

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Fiber.with_binding key 123 @@ fun () -> trace_key ();;
+Key => 123
- : unit = ()
```

`with_binding` will shadow variables defined in outer scopes.

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Fiber.with_binding key 123 @@ fun () ->
  trace_key ();
  Fiber.with_binding key 456 (fun () -> trace_key ());
  trace_key ();;
+Key => 123
+Key => 456
+Key => 123
- : unit = ()
```

Values are propagated when forking:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Fiber.with_binding key 123 @@ fun () ->
  Switch.run @@ fun sw ->
  Fiber.fork ~sw trace_key;;
+Key => 123
- : unit = ()
```

Bindings can also be removed:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Fiber.with_binding key 123 @@ fun () ->
  trace_key ();
  Fiber.without_binding key (fun () -> trace_key ());
  trace_key ();;
+Key => 123
+Key => <unset>
+Key => 123
- : unit = ()
```

Values are inherited from the currently running fiber, rather than the switch.

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Switch.run @@ fun sw ->
  Fiber.with_binding key 123 @@ fun () ->
  Fiber.fork ~sw trace_key;;
+Key => 123
- : unit = ()
```
