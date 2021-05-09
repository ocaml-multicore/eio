# eioio -- effects based parallel IO for OCaml

This library implements an effects-based direct-style IO
stack for multicore OCaml.

The library is very much a work-in-progress, so this is an
unreleased repository.

## Motivation

The `Unix` library provided with OCaml uses blocking IO operations, and is not well suited to concurrent programs such as network services or interactive applications.
For many years, the solution to this has been libraries such as Lwt and Async, which provide a monadic interface.
These libraries allow writing code as if there were multiple threads of execution, each with their own stack, but the stacks are simulated using the heap.

The multicore version of OCaml adds support for "effects", removing the need for monadic code here.
Using effects brings several advantages:

1. It is faster, because no heap allocations are needed to simulate a stack.
2. Concurrent code can be written in the same style as plain non-concurrent code.
3. Because a real stack is used, backtraces from exceptions work as expected.
4. Other features of the language (such as `try ... with ...`) can be used in concurrent code.

In addition, modern operating systems provide high-performance alternatives to the old Unix `select` call.
For example, Linux's io-uring system has applications write the operations they want to perform to a ring buffer,
which Linux handles asynchronously.

Due to this, we anticipate many OCaml users will want to rewrite their IO code at some point,
once effects have been merged into the the official version of OCaml.
It would be very beneficial if we could use this opportunity to standardise on a single concurrency API for OCaml.

This project is therefore exploring what this new API should look like by building an effects-based IO library and
then using it to create or port larger applications.

The API is expected to change a great deal over the next year or so.
If you are looking for a stable library for your application, you should continue using Lwt or Async for now.
However, if you'd like to help with these experiments, please get in touch!

At present, Linux with io-uring is the only backend available.
It is able to run a web-server with good performance, but most features are still missing.

## Structure of the code

- `fibreslib` provides concurrency primitives (promises, semaphores, etc).
- `eio` provides a high-level, cross-platform OS API.
- `eunix` provides a Linux io-uring backend for these APIs,
  plus a low-level API that can be used directly (in non-portable code).
- `ctf` provides tracing support.

## Getting started

Here's a slightly complicated way of writing a greeting to stdout:

```ocaml
# #require "eunix";;

# let main stdenv =
    let src = Eio.Source.of_string "Hello, world!\n" in
    let dst = Eio.Stdenv.stdout stdenv in
    Eio.Sink.write dst ~src;;
val main : < stdout : Eio.Sink.t; .. > -> unit = <fun>

# Eunix.run main;;
- : unit = ()
# (* prints "Hello, world!" *)
```

Note that:

- The program provides its `main` function to `Eunix.run` (which runs the main event loop).

- The `stdenv` argument represents the standard environment of a Unix process, allowing it to interact with the outside world.

- A `main` function will typically start by extracting from `stdenv` whatever things the program will need.

- The type of the `main` function here tells us that this program only interacts via `stdout`.

## Tracing

The library can write traces in CTF format, showing when threads (fibres) are created, when they run, and how they interact.
To turn tracing on, use code like this in your application:

```ocaml
let () =
  let buffer = Ctf.Unix.mmap_buffer ~size:0x100000 "trace.ctf" in
  let trace_config = Ctf.Control.make buffer in
  Ctf.Control.start trace_config;
```

It will then write trace events to the `trace.ctf` file.
The trace can be viewed using [mirage-trace-viewer][].
This should work even while the program is still running.
The file is a ring buffer, so when it gets full old events will start to be overwritten with new ones.

[mirage-trace-viewer]: https://github.com/talex5/mirage-trace-viewer

## Further reading

Some background about the effects system can be found in:

- ["Retrofitting Concurrency onto OCaml"](https://kcsrk.info/papers/retro-concurrency_pldi_21.pdf) (to appear, PLDI 2021)
- https://kcsrk.info/ocaml/multicore/2015/05/20/effects-multicore/
- Effects examples: https://github.com/ocaml-multicore/effects-examples/tree/master/aio
- [Concurrent System Programming with Effect Handlers](https://www.repository.cam.ac.uk/bitstream/handle/1810/283239/paper.pdf?sequence=3&isAllowed=y)
- [Asynchronous effect based IO using effect handlers](https://github.com/kayceesrk/ocaml-aeio)
