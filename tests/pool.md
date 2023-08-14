# Setting up the environment

```ocaml
# #require "eio.mock";;
```

```ocaml
open Eio.Std

module P = Eio.Pool

let dispose x = traceln "disposing %d" x

let create ?validate ?dispose n items =
  let items = Array.of_list items in
  let i = ref 0 in
  P.create ?validate ?dispose n (fun () -> 
    traceln "Creating item %d" !i;
    let p = items.(!i) in
    incr i;
    Promise.await_exn p
  )
```

# Test cases

Simple case:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let t = create 1 [Promise.create_resolved (Ok 0)] in
  P.use t (fun x -> traceln "Using item %d" x);
  P.use t (fun x -> traceln "Using item %d" x);
+Creating item 0
+Using item 0
+Using item 0
- : unit = ()
```

Two uses with a capacity of 1; the second must wait:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let p, r = Promise.create () in
  let t = create 1 [p] in
  Fiber.all [
    (fun () -> P.use t (fun x -> traceln "A: using item %d" x; Fiber.yield (); traceln "A done"));
    (fun () -> P.use t (fun x -> traceln "B: using item %d" x; Fiber.yield (); traceln "B done"));
    (fun () -> Promise.resolve r (Ok 0));
  ];
+Creating item 0
+A: using item 0
+A done
+B: using item 0
+B done
- : unit = ()
```

Two uses with a capacity of 2; they run in parallel:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let p0, r0 = Promise.create () in
  let p1, r1 = Promise.create () in
  let t = create 2 [p0; p1] in
  Fiber.all [
    (fun () -> P.use t (fun x -> traceln "A: using item %d" x; Fiber.yield (); traceln "A done"));
    (fun () -> P.use t (fun x -> traceln "B: using item %d" x; Fiber.yield (); traceln "B done"));
    (fun () -> Promise.resolve r0 (Ok 0); Promise.resolve r1 (Ok 1));
  ];
+Creating item 0
+A: using item 0
+Creating item 1
+B: using item 1
+A done
+B done
- : unit = ()
```

## Cancellation

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let p, r = Promise.create () in
  let t = create 1 [p] in
  Fiber.all [
    (fun () -> P.use t (fun _ -> assert false));    (* Waits for the creation to finish *)
    (fun () -> P.use t (fun _ -> assert false));    (* Waits for the item to be returned *)
    (fun () -> failwith "Simulated error");
  ];
+Creating item 0
Exception: Failure "Simulated error".
```

## Error handling

On error, the resource is still returned to the pool:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let t = create 1 [Promise.create_resolved (Ok 0)] in
  begin
    try P.use t (fun x -> traceln "Using item %d" x; failwith "Simulated error")
    with Failure msg -> traceln "Failed: %s" msg
  end;
  P.use t (fun x -> traceln "Using item %d" x);
+Creating item 0
+Using item 0
+Failed: Simulated error
+Using item 0
- : unit = ()
```

Two fibers are trying to use a resource and one is being created.
When the creation function fails, the first fiber reports the error,
and also wakes the second fiber, which tries again:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let p, r = Promise.create () in
  let t = create 1 [p; Promise.create_resolved (Ok 1)] in
  Switch.run @@ fun sw ->
  let a = Fiber.fork_promise ~sw (fun () -> P.use t (fun i -> traceln "A: using item %d" i)) in
  Fiber.both
    (fun () -> P.use t (fun i -> traceln "B: using item %d" i))
    (fun () -> Promise.resolve_error r (Failure "Simulated creation failure"));
  Promise.await_exn a
+Creating item 0
+Creating item 1
+B: using item 1
Exception: Failure "Simulated creation failure".
```

## Validation

The second time a resource is used, we check it is still valid:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let validate x =
    let ok = (x land 1) = 0 in
    traceln "validate %d => %b" x ok;
    ok
  in
  let t = create ~validate ~dispose 2 [
    Promise.create_resolved (Ok 0);
    Promise.create_resolved (Ok 1);
    Promise.create_resolved (Ok 2);
  ] in
  Fiber.all [
    (fun () -> P.use t (fun x -> traceln "A: using item %d" x; Fiber.yield ()));
    (fun () -> P.use t (fun x -> traceln "B: using item %d" x; Fiber.yield ()));
    (fun () -> P.use t (fun x -> traceln "C: using item %d" x; Fiber.yield ()));
    (fun () -> P.use t (fun x -> traceln "D: using item %d" x; Fiber.yield ()));
  ]
+Creating item 0
+A: using item 0
+Creating item 1
+B: using item 1
+validate 0 => true
+C: using item 0
+validate 1 => false
+disposing 1
+Creating item 2
+D: using item 2
- : unit = ()
```

Dispose fails. We report the error, but still recreate the resource next time:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let validate x =
    let ok = (x land 1) = 1 in
    traceln "validate %d => %b" x ok;
    ok
  in
  let dispose x = Fmt.failwith "Simulated error disposing %d" x in
  let t = create ~validate ~dispose 1 [
    Promise.create_resolved (Ok 0);
    Promise.create_resolved (Ok 1);
    Promise.create_resolved (Ok 2);
  ] in
  begin
    try
      Fiber.both
        (fun () -> P.use t (fun x -> traceln "A: using item %d" x; Fiber.yield ()))
        (fun () -> P.use t (fun x -> traceln "B: using item %d" x; Fiber.yield ()))
      with Failure msg -> traceln "Failed: %s" msg
  end;
  Fiber.both
    (fun () -> P.use t (fun x -> traceln "C: using item %d" x; Fiber.yield ()))
    (fun () -> P.use t (fun x -> traceln "D: using item %d" x; Fiber.yield ()))
+Creating item 0
+A: using item 0
+validate 0 => false
+Failed: Simulated error disposing 0
+Creating item 1
+C: using item 1
+validate 1 => true
+D: using item 1
- : unit = ()
```
