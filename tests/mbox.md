# Mailboxes

```ocaml
# #require "eio_main";;
```

```ocaml
open Eio.Std

module Mbox = Eio.Mbox

let mbox = Mbox.create ()
let recv () = traceln "%d" (Mbox.recv mbox)
let send x () = Mbox.send mbox x

let run fn =
  Eio_main.run @@ fun _ ->
  fn ()
```

# Test cases

Simple case:

```ocaml
# run @@ fun () ->
  let mbox = Mbox.create () in
  Fiber.both
    (fun () -> Mbox.send mbox "got mail")
    (fun () -> traceln "%s" (Mbox.recv mbox));;
+got mail
- : unit = ()
```

Single Producer Multiple Consumer (SPMC)

```ocaml
# run @@ fun () ->
  Fiber.both
    (fun () -> for i = 0 to (pred 5); do send i (); done)
    (fun () -> Fiber.all (List.init 5 (fun _ -> recv)));;
+0
+1
+2
+3
+4
- : unit = ()
```

Multiple Producer Single Consumer (MPSC)

```ocaml
# run @@ fun () ->
  Fiber.both
    (fun () -> Fiber.all (List.init 5 (fun i -> send i)))
    (fun () -> for i = 0 to (pred 5); do recv (); done);;
+0
+1
+2
+3
+4
- : unit = ()
```

Multiple Producer Multiple Consumer (MPMC)

```ocaml
# run @@ fun () ->
  Fiber.both
    (fun () -> Fiber.all (List.init 5 (fun i -> send i)))
    (fun () -> Fiber.all (List.init 5 (fun _ -> recv)));;
+0
+1
+2
+3
+4
- : unit = ()
```

# Cancellation

```ocaml
# run @@ fun () ->
  Fiber.first
    (fun () -> send 0 ())
    (fun () -> Fiber.yield (); recv ());
  recv ();
  Fiber.first
    (fun () -> send 1 ())
    (fun () -> recv ());
  send 2 ();
  recv ();;
+0
+1
+2
- : unit = ()
```
