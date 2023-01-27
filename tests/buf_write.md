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
  Fiber.both
    (fun () ->
       Write.with_flow flow @@ fun t ->
       Write.string t "Hello"; traceln "Did write"
    )
    (fun () -> Fiber.yield (); failwith "Simulated error");;
+Did write
Exception: Failure "Simulated error".
```

Cancelled while waiting for some data:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Switch.run @@ fun sw ->
  let t = Write.create ~sw 100 in
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
let foobar ~sw =
  let t = Write.create ~sw 100 in
  Write.string t "foo";
  Write.cstruct t (Cstruct.of_string "bar");
  Write.close t;
  t
```

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Switch.run @@ fun sw ->
  Write.serialize (foobar ~sw) @@ fun bufs ->
  traceln "Write %a" Fmt.(Dump.list (using Cstruct.to_string Dump.string)) bufs;
  Ok (Cstruct.lenv bufs);;
+Write ["foobar"]
- : (unit, [> `Closed ]) result = Ok ()
```

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Switch.run @@ fun sw ->
  Write.serialize (foobar ~sw) @@ fun bufs ->
  assert (bufs <> []);
  traceln "Write %a" Fmt.(Dump.list (using Cstruct.to_string Dump.string)) bufs;
  Error `Closed;;
+Write ["foobar"]
- : (unit, [> `Closed ]) result = Error `Closed
```

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Switch.run @@ fun sw ->
  Write.serialize_to_string (foobar ~sw);;
- : string = "foobar"
```

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Switch.run @@ fun sw ->
  Write.serialize_to_cstruct (foobar ~sw) |> Cstruct.to_string;;
- : string = "foobar"
```

## Exceptions

We still flush the output on error:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Eio_mock.Flow.on_copy_bytes flow [`Return 1; `Yield_then (`Return 1)];
  Write.with_flow flow @@ fun t ->
  Write.string t "foo";
  failwith "Simulated error";;
+flow: wrote "f"
+flow: wrote "o"
+flow: wrote "o"
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

## Cleanup

Ensure that we don't lose flushing fibers if the writer is aborted:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Switch.run @@ fun main_sw ->
  Switch.run (fun sw ->
     let t = Write.create ~sw 100 in
     Fiber.fork ~sw:main_sw
       (fun () ->
          Write.string t "foo";
          try Write.flush t; assert false
          with ex -> traceln "Flush failed: %a" Fmt.exn ex
       );
     traceln "Finishing writer switch"
  );
  Fiber.yield ();
  traceln "Finishing main switch";;
+Finishing writer switch
+Flush failed: Eio__Buf_write.Flush_aborted
+Finishing main switch
- : unit = ()
```

And with `with_flow`:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Eio_mock.Flow.on_copy_bytes flow [`Raise (Failure "Simulated IO error")];
  Switch.run @@ fun sw ->
  Write.with_flow flow @@ fun t ->
  Fiber.fork ~sw (fun () ->
     Write.string t "foo";
     try Write.flush t; assert false
     with ex -> traceln "Flush failed: %a" Fmt.exn ex
  );
  traceln "with_flow returning; t will be closed";;
+with_flow returning; t will be closed
+Flush failed: Eio__Buf_write.Flush_aborted
Exception: Failure "Simulated IO error".
```

But the flush does succeed in the normal case:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Eio_mock.Flow.on_copy_bytes flow [`Yield_then (`Return 2); `Return 1];
  Switch.run @@ fun sw ->
  Write.with_flow flow @@ fun t ->
  Fiber.fork ~sw (fun () ->
     Write.string t "foo";
     Write.flush t;
     traceln "Flush succeeded"
  );
  traceln "with_flow returning; t should be closed but not aborted";;
+with_flow returning; t should be closed but not aborted
+flow: wrote "fo"
+flow: wrote "o"
+Flush succeeded
- : unit = ()
```

If we don't pass a switch then we can still cancel flushes manually:

```ocaml
# let t = Write.create 100 in
  Eio_mock.Backend.run @@ fun () ->
  Fiber.both
    (fun () ->
       try Write.string t "hi"; Write.flush t
       with Write.Flush_aborted -> traceln "Aborted"
    )
    (fun () -> Write.abort t);;
+Aborted
- : unit = ()
```

## Round-trips with Buf_read

```ocaml
module Read = Eio.Buf_read

let test (x : 'a) (f : Write.t -> 'a -> unit) (g : Read.t -> 'a) =
  let encoded =
    let t = Write.create 10 in
    f t x;
    Write.serialize_to_string t
  in
  let decoded = Read.parse_string_exn g encoded in
  if x <> decoded then Fmt.failwith "Failed to round-trip: %S" encoded;
  encoded

let to_hex data =
  let b = Buffer.create (String.length data * 2) in
  data |> String.iter (fun c ->
    Buffer.add_string b (Printf.sprintf "%02x" (Char.code c))
  );
  Buffer.contents b
```

```ocaml
# test "test" (Write.string ?off:None ?len:None) Read.take_all;;
- : string = "test"

# test '\253' Write.char Read.any_char |> String.escaped;;
- : string = "\\253"

# test 0xa5 Write.uint8 Read.uint8 |> to_hex;;
- : string = "a5"
```

Big-endian:

```ocaml
# test 0xa123 Write.BE.uint16 Read.BE.uint16 |> to_hex;;
- : string = "a123"

# test 0xa1234567l Write.BE.uint32 Read.BE.uint32 |> to_hex;;
- : string = "a1234567"

# test 0xa1234567890aL Write.BE.uint48 Read.BE.uint48 |> to_hex;;
- : string = "a1234567890a"

# test 0xa1234567890abcdeL Write.BE.uint64 Read.BE.uint64 |> to_hex;;
- : string = "a1234567890abcde"

# test 32.25 Write.BE.float Read.BE.float |> to_hex;;
- : string = "42010000"

# test 32.25 Write.BE.double Read.BE.double |> to_hex;;
- : string = "4040200000000000"
```

Little-endian (using `to_hex'` to reverse the output):

```ocaml
let to_hex' d =
  let l = String.length d in
  String.init l (fun i -> d.[l - i - 1])
  |> to_hex
```

```ocaml
# test 0xa123 Write.LE.uint16 Read.LE.uint16 |> to_hex';;
- : string = "a123"

# test 0xa1234567l Write.LE.uint32 Read.LE.uint32 |> to_hex';;
- : string = "a1234567"

# test 0xa1234567890aL Write.LE.uint48 Read.LE.uint48 |> to_hex';;
- : string = "a1234567890a"

# test 0xa1234567890abcdeL Write.LE.uint64 Read.LE.uint64 |> to_hex';;
- : string = "a1234567890abcde"

# test 32.25 Write.LE.float Read.LE.float |> to_hex';;
- : string = "42010000"

# test 32.25 Write.LE.double Read.LE.double |> to_hex';;
- : string = "4040200000000000"
```
