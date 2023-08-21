# Setting up the environment

```ocaml
# #require "eio.mock";;
```
```ocaml
open Eio.Std

let test label v =
  traceln "%s: forcing..." label;
  match Eio.Lazy.force v with
  | v ->
    Fiber.check ();
    traceln "%s: %d" label v
  | exception ex ->
    traceln "%s: %a" label Fmt.exn ex;
    Fiber.check ()
```

# Tests

Two fibers request the value. It's only computed once:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let v = Eio.Lazy.from_fun ~cancel:`Restart (fun () ->
    traceln "calculating...";
    Fiber.yield ();
    traceln "complete";
    42
  ) in
  Fiber.both
    (fun () -> test "a" v)
    (fun () -> test "b" v)
  ;;
+a: forcing...
+calculating...
+b: forcing...
+complete
+a: 42
+b: 42
- : unit = ()
```

The calculation fails. It's still only performed once:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let v = Eio.Lazy.from_fun ~cancel:`Restart (fun () ->
    traceln "calculating...";
    Fiber.yield ();
    failwith "failed";
  ) in
  Fiber.both
    (fun () -> test "a" v)
    (fun () -> test "b" v)
  ;;
+a: forcing...
+calculating...
+b: forcing...
+a: Failure("failed")
+b: Failure("failed")
- : unit = ()
```

## Cancellation

The first fiber cancels. What happens depends on the cancel mode:

```ocaml
let test_cancel cancel =
  Eio_mock.Backend.run @@ fun () ->
  let v = Eio.Lazy.from_fun ~cancel (fun () ->
    traceln "calculating...";
    Fiber.yield ();
    traceln "complete";
    42
  ) in
  Fiber.both
    (fun () ->
       let x =
         Fiber.first
           (fun () -> test "a" v; assert false)
           (fun () -> 5)
       in
       traceln "a: %d" x
    )
    (fun () -> test "b" v)
  ;;
```

In record mode, the second fiber sees the cancelled exception:

```ocaml
# test_cancel `Record;;
+a: forcing...
+calculating...
+b: forcing...
+a: Cancelled: Eio__core__Fiber.Not_first
+b: Cancelled: Eio__core__Fiber.Not_first
+a: 5
- : unit = ()
```

In protect mode, the first calculation succeeds:

```ocaml
# test_cancel `Protect;;
+a: forcing...
+calculating...
+b: forcing...
+complete
+b: 42
+a: 5
- : unit = ()
```

In restart mode, the second fiber restarts the calculation:

```ocaml
# test_cancel `Restart;;
+a: forcing...
+calculating...
+b: forcing...
+a: Cancelled: Eio__core__Fiber.Not_first
+calculating...
+a: 5
+complete
+b: 42
- : unit = ()
```
