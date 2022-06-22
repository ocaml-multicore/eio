```ocaml
# #require "eio";;
# #require "eio.mock";;
```
```ocaml
open Eio.Std

module Write = Eio.Buf_write

let flow = Eio_mock.Flow.make "flow"

let flow_rsb = Eio_mock.Flow.make "flow"
let () = Eio_mock.Flow.set_copy_method flow_rsb `Read_source_buffer
```

## A simple run-through

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Write.with_flow flow @@ fun w ->
  Write.string w "Hello"; Write.char w ' '; Write.string w "world";;
+flow: wrote "Hello world"
- : unit = ()
```

## Auto-commit

If we yield then we flush the data so far:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Write.with_flow flow @@ fun w ->
  Write.string w "Hello"; Write.char w ' ';
  Fiber.yield ();
  Write.string w "world";;
+flow: wrote "Hello "
+flow: wrote "world"
- : unit = ()
```

## Read source buffer

If supported by the flow, we can avoid copying:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Write.with_flow flow_rsb @@ fun w ->
  Write.string w "Hello";
  Write.char w ' ';
  Write.schedule_cstruct w (Cstruct.of_string "world");
  Write.char w '!';;
+flow: wrote (rsb) ["Hello "; "world"; "!"]
- : unit = ()
```

## Pausing

Without pausing:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Write.with_flow flow @@ fun w ->
  Write.string w "Hello... ";
  Fiber.yield ();
  Write.string w "world";;
+flow: wrote "Hello... "
+flow: wrote "world"
- : unit = ()
```

With pausing

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Write.with_flow flow @@ fun w ->
  Write.string w "Hello... ";
  Write.pause w;
  Fiber.yield ();
  Write.unpause w;
  Write.string w "world";;
+flow: wrote "Hello... world"
- : unit = ()
```

## Empty writes

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Write.with_flow flow @@ fun t ->
  Write.string t "";
  Write.bytes t (Bytes.make 0 '\000');
  Write.cstruct t Cstruct.empty;
  Write.schedule_cstruct t Cstruct.empty;;
- : unit = ()
```

## Endianness

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Write.with_flow flow @@ fun t ->
  Write.LE.uint16 t 5;
  Fiber.yield ();
  Write.BE.uint16 t 5;;
+flow: wrote "\005\000"
+flow: wrote "\000\005"
- : unit = ()
```

## Writes

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Write.with_flow flow @@ fun t ->
  Write.string t "test";
  Fiber.yield ();
  Write.bytes  t (Bytes.of_string "test");
  Fiber.yield ();
  Write.cstruct t (Cstruct.of_string ~off:1 ~len:4 "!test!");
  Fiber.yield ();
  Write.char t 'A';;;
+flow: wrote "test"
+flow: wrote "test"
+flow: wrote "test"
+flow: wrote "A"
- : unit = ()
```

## Multiple writes

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let f t =
    Write.string t "te";
    Write.string t "st";
    Write.string t "te";
    Write.string t "st";
    Write.char   t 't';
    Write.char   t 'e'
  in
  traceln "With room:";
  Write.with_flow flow_rsb f;
  traceln "Without room:";
  Write.with_flow ~initial_size:1 flow_rsb f;;
+With room:
+flow: wrote (rsb) ["testtestte"]
+Without room:
+flow: wrote (rsb) ["te"; "st"; "te"; "st"; "te"]
- : unit = ()
```

## Flushing

```ocaml
let p1, r2 = Promise.create ();;

Eio_mock.Flow.on_copy_bytes flow [
  `Await p1;
]
```

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Write.with_flow flow @@ fun t ->
  Fiber.both
    (fun () ->
       Write.flush t;
       Write.string t "Hello";
       traceln "Flushing...";
       Write.flush t;
       traceln "Flushed"
    )
    (fun () ->
       traceln "Write now completes...";
       Promise.resolve_ok r2 3
    );;
+Flushing...
+Write now completes...
+flow: wrote "Hel"
+flow: wrote "lo"
+Flushed
- : unit = ()
```

Multiple flushes:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Eio_mock.Flow.on_copy_bytes flow_rsb [
    `Yield_then (`Return 1);
    `Yield_then (`Return 2);
    `Yield_then (`Return 2);
    `Yield_then (`Return 2);
  ];
  Write.with_flow flow_rsb @@ fun t ->
  Fiber.all [
    (fun () -> Write.string t "ab"; Write.flush t; traceln "1st flush");
    (fun () -> Write.string t "cd"; Write.flush t; traceln "2nd flush");
    (fun () -> Write.string t "ef"; Write.flush t; traceln "3rd flush");
  ];
  traceln "Done";;
+flow: wrote (rsb) ["a"]
+flow: wrote (rsb) ["b"; "c"]
+1st flush
+flow: wrote (rsb) ["d"; "e"]
+2nd flush
+flow: wrote (rsb) ["f"]
+3rd flush
+Done
- : unit = ()
```

## Scheduling

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Write.with_flow flow @@ fun t ->
  Write.schedule_cstruct t (Cstruct.of_string "one");
  Write.string t "two";
  Fiber.yield ();
  Write.string t "one";
  Write.schedule_cstruct t (Cstruct.of_string "two");
  Fiber.yield ();
  Write.schedule_cstruct t (Cstruct.of_string "end");
  Fiber.yield ();
  traceln "Should all be flushed by now.";;;
+flow: wrote "onetwo"
+flow: wrote "onetwo"
+flow: wrote "end"
+Should all be flushed by now.
- : unit = ()
```

## Cancellation

Cancelled while waiting for the underlying flow to perform the write:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let flow = Eio_mock.Flow.make "flow" in
  Eio_mock.Flow.on_copy_bytes flow [`Run Fiber.await_cancel];
  Write.with_flow flow @@ fun t ->
  Fiber.both
    (fun () -> Write.string t "Hello"; traceln "Did write")
    (fun () -> Fiber.yield (); failwith "Simulated error");;
+Did write
Exception: Failure "Simulated error".
```

Cancelled while waiting for some data:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let t = Write.create 100 in
  Fiber.both
    (fun () -> ignore (Write.await_batch t); assert false)
    (fun () -> failwith "Simulated error");;
Exception: Failure "Simulated error".
```

## Invalid offset

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Write.with_flow flow @@ fun t ->
  try Write.string t "hi" ~off:100; assert false
  with Invalid_argument _ -> ();;
- : unit = ()
```

## Serialize

```ocaml
let foobar () =
  let t = Write.create 100 in
  Write.string t "foo";
  Write.cstruct t (Cstruct.of_string "bar");
  Write.close t;
  t
```

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Write.serialize (foobar ()) @@ fun bufs ->
  traceln "Write %a" Fmt.(Dump.list (using Cstruct.to_string Dump.string)) bufs;
  Ok (Cstruct.lenv bufs);;
+Write ["foobar"]
- : (unit, [> `Closed ]) result = Ok ()
```

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Write.serialize (foobar ()) @@ fun bufs ->
  assert (bufs <> []);
  traceln "Write %a" Fmt.(Dump.list (using Cstruct.to_string Dump.string)) bufs;
  Error `Closed;;
+Write ["foobar"]
- : (unit, [> `Closed ]) result = Error `Closed
```

```ocaml
# Write.serialize_to_string (foobar ());;
- : string = "foobar"
```

```ocaml
# Write.serialize_to_cstruct (foobar ()) |> Cstruct.to_string;;
- : string = "foobar"
```

## Exceptions

We still flush the output on error:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Write.with_flow flow @@ fun t ->
  Write.string t "foo";
  failwith "Simulated error";;
+flow: wrote "foo"
Exception: Failure "Simulated error".
```

But we don't flush if cancelled:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let flow = Eio_mock.Flow.make "flow" in
  Eio_mock.Flow.on_copy_bytes flow [`Run Fiber.await_cancel];
  Fiber.both
    (fun () ->
       Write.with_flow flow @@ fun t ->
       Write.string t "foo";
       Fiber.await_cancel ()
    )
    (fun () -> failwith "Simulated error");;
Exception: Failure "Simulated error".
```
