```ocaml
# #require "eio";;
# #require "eio.mock";;
```
```ocaml
open Eio.Std

module Write = Eio.Buf_write

let flow = Eio_mock.Flow.make "flow"
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
  Write.with_flow flow @@ fun w ->
  Write.string w "Hello";
  Write.char w ' ';
  Write.schedule_cstruct w (Cstruct.of_string "world");
  Write.char w '!';;
+flow: wrote ["Hello "; "world"; "!"]
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
+flow: wrote ["Hello... "; "world"]
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
  Write.with_flow flow f;
  traceln "Without room:";
  Write.with_flow ~initial_size:1 flow f;;
+With room:
+flow: wrote "testtestte"
+Without room:
+flow: wrote ["te"; "st"; "te"; "st"; "te"]
- : unit = ()
```

## Formatting

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Write.with_flow flow @@ fun t ->
  Write.printf t "Write.printf can force a full flush@.@[<v2>It also@,flushes to [t] automatically";
  Write.string t " at the end, but without flushing [t] itself.\n";
  (* Create a formatter for full control: *)
  let f = Write.make_formatter t in
  Format.pp_set_geometry f ~max_indent:4 ~margin:10;
  (*
    "@ "      breakable space
    "@[<v 6>" open vertical box, indentation: 6 (overriden by our geometry settings)
    "%s"      print string
    "@ "      breakable space
    "%i"      print int
    "@."      print newline + explicit flush
    "%a"      print arbitrary type
    "@]"      close box
    "@ "      breakable space
  *)
  Fmt.pf f "Space@ @[<v 6>%s@ %i@.%a@]@ "
    "This is a test" 123
    Eio.Net.Sockaddr.pp (`Tcp (Eio.Net.Ipaddr.V6.loopback, 8080));
  Write.printf t "This is a %s call to printf" "second";
  Fmt.pf f "@.Flushed. %s@." "Goodbye"
+flow: wrote "Write.printf can force a full flush\n"
+flow: wrote "It also\n"
+            "  flushes to [t] automatically at the end, but without flushing [t] itself.\n"
+            "Space\n"
+            "This is a test\n"
+            "    123\n"
+flow: wrote "tcp:[::1]:8080This is a second call to printf\n"
+            "\n"
+flow: wrote "Flushed. Goodbye\n"
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
  Eio_mock.Flow.on_copy_bytes flow [
    `Yield_then (`Return 1);
    `Yield_then (`Return 2);
    `Yield_then (`Return 2);
    `Yield_then (`Return 2);
  ];
  Write.with_flow flow @@ fun t ->
  Fiber.all [
    (fun () -> Write.string t "ab"; Write.flush t; traceln "1st flush");
    (fun () -> Write.string t "cd"; Write.flush t; traceln "2nd flush");
    (fun () -> Write.string t "ef"; Write.flush t; traceln "3rd flush");
  ];
  traceln "Done";;
+flow: wrote "a"
+flow: wrote ["b"; "c"]
+1st flush
+flow: wrote ["d"; "e"]
+2nd flush
+flow: wrote "f"
+3rd flush
+Done
- : unit = ()
```

Check flush waits for the write to succeed:

```ocaml
module Slow_writer = struct
  type t = unit

  let copy t ~src =
    let buf = Cstruct.create 10 in
    try
      while true do
        let len = Eio.Flow.single_read src buf in
        Fiber.yield ();
        traceln "Write %S" (Cstruct.to_string buf ~len)
      done
    with End_of_file -> ()

  let single_write t bufs =
    copy t ~src:(Eio.Flow.cstruct_source bufs);
    Cstruct.lenv bufs
end
let slow_writer =
  let ops = Eio.Flow.Pi.sink (module Slow_writer) in
  Eio.Resource.T ((), ops)
```

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Write.with_flow slow_writer @@ fun t ->
  Write.string t "test";
  Write.flush t;
  traceln "Flush complete"
+Write "test"
+Flush complete
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
+flow: wrote ["one"; "two"]
+flow: wrote ["one"; "two"]
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
