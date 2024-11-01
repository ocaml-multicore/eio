# Setting up the environment

```ocaml
# #require "eio.mock";;
```

```ocaml
open Eio.Std

module T = Eio.Semaphore

let run fn =
  Eio_mock.Backend.run @@ fun _ ->
  fn ()

let acquire t =
  traceln "Acquiring";
  T.acquire t;
  traceln "Acquired"

let release t =
  traceln "Releasing";
  T.release t;
  traceln "Released"
```

# Test cases

Simple case:

```ocaml
# run @@ fun () ->
  let t = T.make 1 in
  acquire t;
  release t;
  acquire t;
  release t;;
+Acquiring
+Acquired
+Releasing
+Released
+Acquiring
+Acquired
+Releasing
+Released
- : unit = ()
```

Concurrent access to the semaphore:

```ocaml
# run @@ fun () ->
  let t = T.make 2 in
  let fn () =
    acquire t;
    Eio.Fiber.yield ();
    release t
  in
  List.init 4 (fun _ -> fn)
  |> Fiber.all;;
+Acquiring
+Acquired
+Acquiring
+Acquired
+Acquiring
+Acquiring
+Releasing
+Released
+Releasing
+Released
+Acquired
+Acquired
+Releasing
+Released
+Releasing
+Released
- : unit = ()
```

Cancellation:

```ocaml
# run @@ fun () ->
  let t = T.make 0 in
  Fiber.first
    (fun () -> acquire t)
    (fun () -> ());
  release t;
  acquire t;;
+Acquiring
+Releasing
+Released
+Acquiring
+Acquired
- : unit = ()
```
