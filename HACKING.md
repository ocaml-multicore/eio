## Installing Eio from Git

If you want to run the latest development version from Git, run these commands:

```
git clone https://github.com/ocaml-multicore/eio.git
cd eio
opam pin -yn .
opam install eio_main
```

## Layout of the code

`lib_eio/core` contains the core logic about fibers, promises, switches, etc.
`lib_eio` extends this with e.g. streams, buffered readers, buffered writers,
and a load of types for OS resources (files, networks, etc).

There is one directory for each backend (e.g. `eio_linux`).
Each backend provides a scheduler that integrates with a particular platform,
and implements some or all of the cross-platform resource APIs.
For example, `eio_linux` implements the network interface using `io_uring` to send data.

`lib_main` just selects an appropriate backend for the current system.

## Writing a backend

It's best to start by reading `lib_eio/mock/backend.ml`, which implements a mock backend with no actual IO.
You can then read one of the real backends to see how to integrate this with the OS.

Most backends are built in two layers:

- A "low-level" module directly wraps the platform's own API, just adding support for suspending fibers for concurrency
  and basic safety features (such wrapping `Unix.file_descr` to prevent use-after-close races).

- An implementation of the cross-platform API (as defined in the `eio` package) that uses the low-level API internally.
  This should ensure that errors are reported using the `Eio.Io` exception.

`eio_posix` is the best one to look at first:

- `lib_eio_posix/sched.ml` is similar to the mock scheduler, but extended to interact with the OS kernel.
- `lib_eio_posix/low_level.ml` provides fairly direct wrappers of the standard POSIX functions,
  but using `sched.ml` to suspend and resume instead of blocking the whole domain.
- `lib_eio_posix/net.ml` implements the cross-platform API using the low-level API.
  For example, it converts Eio network addresses to Unix ones.
  Likewise, `fs.ml` implements the cross-platform file-system APIs, etc.
- `lib_eio_posix/eio_posix.ml` provides the main `run` function.
  It runs the scheduler, passing to the user's `main` function an `env` object for the cross-platform API functions.

When writing a backend, it's best to write the main loop in OCaml rather than delegate that to a C function.
Some particular things to watch out for:

- If a system call returns `EINTR`, you must switch back to OCaml
  (`caml_leave_blocking_section`) so that the signal can be handled. Some C
  libraries just restart the function immediately and this will break signal
  handling (on systems that have signals).

- If C code installs a signal handler, it *must* use the alt stack (`SA_ONSTACK`).
  Otherwise, signals handlers will run on the fiber stack, which is too small and will result in memory corruption.

- Effects cannot be performed over a C function.
  So, if the user installs an effect handler and then calls a C mainloop, and the C code invokes a callback,
  the callback cannot use the effect handler.
  This isn't a problem for Eio itself (Eio's effect handler is installed inside the mainloop),
  but it can break programs using effects in other ways.

## Tests

Eio has tests in many places...

### Cross-platform unit tests

These are in the top-level `tests` directory.
They are run against whichever backend `Eio_main.run` selects, and therefore must get the same result for all backends.

### Concurrency primitives

`lib_eio/tests` tests some internal data structures, such as the lock-free cells abstraction.
The `.md` files in that directory provide a simple walk-through to demonstrate the basic operation,
while `lib_eio/tests/dscheck` uses [dscheck][] to perform exhaustive testing of all atomic interleavings

At the time of writing, dscheck has some performance problems that make it unusable by default, so
you must use the version in https://github.com/ocaml-multicore/dscheck/pull/3 instead.

### Benchmarks

The `bench` directory contains various speed tests.
`make bench` is a convenient way to run all of them.
This is useful to check for regressions.

If you want to contibute an optimisation, please add a benchmark so that we can measure the improvement.
If you are changing something, make sure the benchmark doesn't get significantly worse.

### Stress and fuzz testing

The `fuzz` directory uses afl-fuzz to search for bugs.

Using it properly requires an instrumented version of the OCaml compiler
(see https://v2.ocaml.org/manual/afl-fuzz.html for instructions).
The `dune` build rules don't use afl-fuzz; they just do a few random tests and then stop.

To run e.g. the `fuzz_buf_read` tests with afl-fuzz:

```
mkdir input
date > input/seed
afl-fuzz -m 1000 -i input -o output ./_build/default/fuzz/fuzz_buf_read.exe @@
```

- `Fork server handshake failed` indicates that you are not using an AFL-enabled version of OCaml.
- `The current memory limit (75.0 MB) is too restrictive` means you forgot to use `-m`.

The `stress` directory contains stress tests (that try to trigger races by brute force).

### Backend-specific tests

There are also backend-specific tests, e.g.

- `lib_eio_linux/tests`
- `lib_eio_luv/tests`

Use these for tests that only make sense for one platform.

## Code formatting

Eio's code is indented using ocp-indent.
When making PRs, please do not apply other formatting tools to existing code unrelated to your PR.
Try to avoid making unnecessary changes; this makes review harder and clutters up the Git history.
`ocamlformat` may be useful to get badly messed up code to a baseline unformatted state,
from which human formatting can be added where needed.

[dscheck]: https://github.com/ocaml-multicore/dscheck
