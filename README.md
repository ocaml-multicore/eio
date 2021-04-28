# eioio -- effects based parallel IO for OCaml

This library implements an effects-based direct-style IO
stack for multicore OCaml.

The library is very much a work-in-progress, so this is an
unreleased repository.

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
