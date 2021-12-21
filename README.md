# Eio -- Effects-Based Parallel IO for OCaml

This library implements an effects-based direct-style IO
stack for multicore OCaml.

This is an unreleased repository, as it's very much a work-in-progress.

## Contents

<!-- vim-markdown-toc GFM -->

* [Motivation](#motivation)
* [Structure of the Code](#structure-of-the-code)
* [Getting Started](#getting-started)
* [Testing with Mocks](#testing-with-mocks)
* [Fibres](#fibres)
* [Tracing](#tracing)
* [Cancellation](#cancellation)
* [Racing](#racing)
* [Switches](#switches)
* [Design Note: Results vs Exceptions](#design-note-results-vs-exceptions)
* [Performance](#performance)
* [Networking](#networking)
* [Design Note: Object Capabilities](#design-note-object-capabilities)
* [Filesystem Access](#filesystem-access)
* [Time](#time)
* [Multicore Support](#multicore-support)
* [Design Note: Thread-Safety](#design-note-thread-safety)
* [Synchronisation Tools](#synchronisation-tools)
  * [Promises](#promises)
  * [Streams](#streams)
  * [Example: a worker pool](#example-a-worker-pool)
* [Design Note: Determinism](#design-note-determinism)
* [Examples](#examples)
* [Further Reading](#further-reading)

<!-- vim-markdown-toc -->

## Motivation

The `Unix` library provided with OCaml uses blocking IO operations, and is not well suited to concurrent programs such as network services or interactive applications.
For many years, the solution to this has been libraries such as Lwt and Async, which provide a monadic interface.
These libraries allow writing code as if there were multiple threads of execution, each with their own stack, but the stacks are simulated using the heap.

The multicore version of OCaml adds support for "effects", removing the need for monadic code here.
Using effects brings several advantages:

1. It's faster, because no heap allocations are needed to simulate a stack.
2. Concurrent code can be written in the same style as plain non-concurrent code.
3. Because a real stack is used, backtraces from exceptions work as expected.
4. Other features of the language (such as `try ... with ...`) can be used in concurrent code.

Additionally, modern operating systems provide high-performance alternatives to the old Unix `select` call.
For example, Linux's io-uring system has applications write the operations they want to perform to a ring buffer,
which Linux handles asynchronously.

Due to this, we anticipate many OCaml users will want to rewrite their IO code at some point,
once effects have been merged into the official version of OCaml.
It would be very beneficial to use this opportunity to standardise a single concurrency API for OCaml.

This project explores what this new API should look like by building an effects-based IO library and
then using it to create or port larger applications.

The API is expected to change a great deal over the next year or so.
If you're looking for a stable library for your application, you should continue using Lwt or Async for now.
However, if you'd like to help with these experiments, please get in touch!

At present, the library provides a generic backend based on libuv, which should work on most platforms,
plus an optimised backend for Linux using io-uring.
It's able to run a web-server with [good performance][http-bench], but many features are still missing.

## Structure of the Code

- `eio` provides concurrency primitives (promises, etc.) and a high-level, cross-platform OS API.
- `eio_luv` provides a cross-platform backend for these APIs using [luv](https://github.com/aantron/luv) (libuv).
- `eio_linux` provides a Linux io-uring backend for these APIs,
  plus a low-level API that can be used directly (in non-portable code).
- `eunix` provides some common code shared by multiple backends.
- `eio_main` selects an appropriate backend (e.g. `eio_linux` or `eio_luv`), depending on your platform.
- `ctf` provides tracing support.

## Getting Started

You'll need a version of the OCaml compiler with effects.
You can get one like this:

```
opam switch create 4.12.0+domains --repositories=multicore=git+https://github.com/ocaml-multicore/multicore-opam.git,default
```

Then you'll need to install this library (and `utop` if you want to try it interactively):

```
git clone https://github.com/ocaml-multicore/eio.git
cd eio
opam pin -yn .
opam depext -i eio_main utop		# (for opam 2.0)
opam install eio_main utop		# (for opam 2.1)
```
(Run `opam --version` if you're not sure which one you have installed.)

Try out the examples interactively by running `utop` in the shell.

First `require` the `eio_main` library. It's also convenient to open the `Eio.Std`
module, as follows. (The leftmost `#` shown below is the Utop prompt, so enter the text after the 
prompt and return after each line.)

```ocaml
# #require "eio_main";;
# open Eio.Std;;
```

This function writes a greeting to `stdout`:

```ocaml
let main ~stdout =
  Eio.Flow.copy_string "Hello, world!\n" stdout
```

We use `Eio_main.run` to run the event loop and call it from there:

```ocaml
# Eio_main.run @@ fun env ->
  main ~stdout:(Eio.Stdenv.stdout env);;
Hello, world!
- : unit = ()
```

Note that:

- The `env` argument represents the standard environment of a Unix process, allowing it to interact with the outside world.
  A program will typically start by extracting from `env` whatever things the program will need and then calling `main` with them.

- The type of the `main` function here tells us that this program only interacts via `stdout`.

- `Eio_main.run` automatically calls the appropriate run function for your platform.
  For example, on Linux this will call `Eio_linux.run`. For non-portable code you can use the platform-specific library directly.

## Testing with Mocks

Because external resources are provided to `main` as arguments, we can easily replace them with mocks for testing.
For example:

```ocaml
# Eio_main.run @@ fun _env ->
  let buffer = Buffer.create 20 in
  main ~stdout:(Eio.Flow.buffer_sink buffer);
  traceln "Main would print %S" (Buffer.contents buffer);;
+Main would print "Hello, world!\n"
- : unit = ()
```

`traceln` provides convenient printf-style debugging, without requiring you to plumb `stderr` through your code.
It's actually using the `Format` module, so you can use the extended formatting directives here too.

## Fibres

Here's an example running two threads of execution (fibres) concurrently:

```ocaml
let main _env =
  Fibre.both
    (fun () -> for x = 1 to 3 do traceln "x = %d" x; Fibre.yield () done)
    (fun () -> for y = 1 to 3 do traceln "y = %d" y; Fibre.yield () done);;
```

```ocaml
# Eio_main.run main;;
+x = 1
+y = 1
+x = 2
+y = 2
+x = 3
+y = 3
- : unit = ()
```

The two fibres run on a single core, so only one can be running at a time.
Calling an operation that performs an effect (such as `yield`) can switch to a different thread.

## Tracing

The library can write traces in CTF format, showing when threads (fibres) are created, when they run, and how they interact.
We can run the previous code with tracing enabled (writing to a new `trace.ctf` file) like this:

```ocaml
# #require "ctf.unix";;
# let () =
    Ctf_unix.with_tracing "trace.ctf" @@ fun () ->
    Eio_main.run main;;
+x = 1
+y = 1
+x = 2
+y = 2
+x = 3
+y = 3
```

The trace can be viewed using [mirage-trace-viewer][].
This should work even while the program is still running.
The file is a ring buffer, so when it gets full, old events will start to be overwritten with new ones.

<p align='center'>
  <img src="./doc/trace.svg"/>
</p>

This shows the two counting threads as two horizonal lines.
The white regions indicate when each thread was running.
Note that the output from `traceln` appears in the trace as well as on the console.

## Cancellation

Every fibre has a cancellation context.
If one of the `Fibre.both` fibres fails, the other is cancelled:

```ocaml
# Eio_main.run @@ fun _env ->
  Fibre.both
    (fun () -> for x = 1 to 3 do traceln "x = %d" x; Fibre.yield () done)
    (fun () -> failwith "Simulated error");;
+x = 1
Exception: Failure "Simulated error".
```

What happened here was:

1. `Fibre.both` created a new cancellation context for the child fibres.
2. The first fibre ran, printed `x = 1` and yielded.
3. The second fibre raised an exception.
4. `Fibre.both` caught the exception and cancelled the context.
5. The first thread's `yield` raised a `Cancelled` exception there.
6. Once both threads had finished, `Fibre.both` re-raised the original exception.

There is a tree of cancellation contexts for each domain, and every fibre is in one context.
When an exception is raised, it propagates towards the root until handled, cancelling the other branches as it goes.
You should assume that any operation that can switch fibres can also raise a `Cancelled` exception if an uncaught exception
reaches one of its ancestor cancellation contexts.

If you want to make an operation non-cancellable, wrap it with `Cancel.protect`
(this creates a new context that isn't cancelled with its parent).

## Racing

`Fibre.first` returns the result of the first fibre to finish, cancelling the other one:

```ocaml
# Eio_main.run @@ fun _env ->
  let x =
    Fibre.first
      (fun () ->
        traceln "first fibre delayed...";
        Fibre.yield ();
        traceln "delay over";
        "a"
      )
      (fun () -> "b")
  in
  traceln "x = %S" x;;
+first fibre delayed...
+x = "b"
- : unit = ()
```

## Switches

A switch is used to group fibres together, so they can be waited on together.
This is a form of [structured concurrency][].
For example:

```ocaml
# Eio_main.run @@ fun _env ->
  Switch.run (fun sw ->
    Fibre.fork ~sw
      (fun () -> for i = 1 to 3 do traceln "i = %d" i; Fibre.yield () done);
    traceln "First thread forked";
    Fibre.fork ~sw
      (fun () -> for j = 1 to 3 do traceln "j = %d" j; Fibre.yield () done);
    traceln "Second thread forked; top-level code is finished"
  );
  traceln "Switch is finished";;
+i = 1
+First thread forked
+j = 1
+Second thread forked; top-level code is finished
+i = 2
+j = 2
+i = 3
+j = 3
+Switch is finished
- : unit = ()
```

`Switch.run fn` creates a new switch `sw` and runs `fn sw`.
`fn` may spawn new fibres and attach them to the switch.
It may also attach other resources such as open file handles.
`Switch.run` waits until `fn` and all other attached fibres have finished, and then
releases any attached resources (e.g. closing all attached file handles).

If you call a function without giving it access to a switch,
then when the function returns you can be sure that any fibres it spawned have finished,
and any files it opened have been closed.
So, a `Switch.run` puts a bound on the lifetime of things created within it,
leading to clearer code and avoiding resource leaks.

For example, `fork` creates a new fibre that continues running after `fork` returns,
so it needs to take a switch argument.

Every switch also creates a new cancellation context,
and you can turn off the switch to cancel all fibres within it.

You can also use `Fibre.fork_sub` to create a child sub-switch.
Turning off the parent switch will also turn off the child switch, but turning off the child doesn't disable the parent.

For example, a web-server might use one switch for the whole server and then create one sub-switch for each incoming connection.
This allows you to end all fibres handling a single connection by turning off that connection's switch,
or to exit the whole application using the top-level switch.

## Design Note: Results vs Exceptions

The OCaml standard library uses exceptions to report errors in most cases.
Many libraries instead use the `result` type, which has the advantage of tracking the possible errors in the type system.
However, using `result` is slower, as it requires more allocations, and explicit code to propagate errors.

As part of the effects work, OCaml is expected to gain a [typed effects][] extension to the type system,
allowing it to track both effects and exceptions statically.
In anticipation of this, the Eio library prefers to use exceptions in most cases,
reserving the use of `result` for cases where the caller is likely to want to handle the problem immediately
rather than propagate it.

## Performance

As mentioned above, Eio allows you to supply your own implementations of its abstract interfaces.
This is in contrast to OCaml's standard library, which only operates on OS file descriptors.
You might wonder what the performance impact of this is.
Here's a simple implementation of `cat` using the standard OCaml functions:

```ocaml
# let () =
    let buf = Bytes.create 4096 in
    let rec copy () =
      match input stdin buf 0 4096 with
      | 0 -> ()
      | got ->
        output stdout buf 0 got;
        copy ()
    in
    copy ();;
```

And here is the equivalent using Eio:

```ocaml
# let () =
    Eio_main.run @@ fun env ->
    Eio.Flow.copy
      (Eio.Stdenv.stdin env)
      (Eio.Stdenv.stdout env);;
```

Testing on a fresh 10G file with [pv](https://www.ivarch.com/programs/pv.shtml) on my machine gives:

```
$ truncate -s 10G dummy

$ cat_ocaml_unix.exe <dummy | pv >/dev/null
10.0GiB 0:00:04 [2.33GiB/s]

$ cat                <dummy | pv >/dev/null
10.0GiB 0:00:04 [2.42GiB/s]

$ cat_ocaml_eio.exe  <dummy | pv >/dev/null
10.0GiB 0:00:03 [3.01GiB/s]
```

`Eio.Flow.copy src dst` asks `dst` to copy from `src`.
As `dst` here is a Unix file descriptor,
it first calls the `probe` method on the `src` object to check whether it is too.
Discovering that `src` is also a file descriptor, it switches to a faster code path optimised for that case.
On my machine, this code path uses the Linux-specific `splice` system call for maximum performance.

Note that not all cases are well-optimised yet, but the idea is for each backend to choose the most efficient way to implement the operation.

## Networking

Eio provides a simple high-level API for networking.
Here is a client that connects to address `addr` using `network` and sends a message:

```ocaml
let run_client ~net ~addr =
  traceln "Connecting to server...";
  Switch.run @@ fun sw ->
  let flow = Eio.Net.connect ~sw net addr in
  Eio.Flow.copy_string "Hello from client" flow
```

Note: the `flow` is attached to `sw` and will be closed automatically when it finishes.

Here is a server that listens on `socket` and handles a single connection by reading a message:

```ocaml
let run_server socket =
  Switch.run @@ fun sw ->
  Eio.Net.accept_sub socket ~sw (fun ~sw flow _addr ->
    traceln "Server accepted connection from client";
    let b = Buffer.create 100 in
    Eio.Flow.copy flow (Eio.Flow.buffer_sink b);
    traceln "Server received: %S" (Buffer.contents b)
  ) ~on_error:(traceln "Error handling connection: %a" Fmt.exn);
  traceln "(normally we'd loop and accept more connections here)"
```

Notes:

- `accept_sub` handles the connection in a new fibre, with its own subswitch.
- Normally, a server would call `accept_sub` in a loop to handle multiple connections.
- When the child switch created by `accept_sub` finishes, `flow` is closed automatically.

We can test them in a single process using `Fibre.both`:

```ocaml
let main ~net ~addr =
  Switch.run @@ fun sw ->
  let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  traceln "Server ready...";
  Fibre.both
    (fun () -> run_server server)
    (fun () -> run_client ~net ~addr)
```

```ocaml
# Eio_main.run @@ fun env ->
  main
    ~net:(Eio.Stdenv.net env)
    ~addr:(`Tcp (Unix.inet_addr_loopback, 8080));;
+Server ready...
+Connecting to server...
+Server accepted connection from client
+(normally we'd loop and accept more connections here)
+Server received: "Hello from client"
- : unit = ()
```

## Design Note: Object Capabilities

The `Eio` high-level API follows the principles of the [Object-capability model][] (Ocaps).
In this model, having a reference to an "object" (which could be a function or closure) grants permission to use it.
The only ways to get a reference are to create a new object, or to be passed an existing reference by another object.
For A to pass a reference B to another object C, A requires access (i.e. references) to both B and C.
In particular, for B to get a reference to C, there must be a path in the reference graph between them
on which all objects allow it.

This is all just standard programming practice, really, except that it disallows patterns that break this model:

- Global variables are not permitted. Otherwise, B could store itself in a global variable and C could collect it.
- Modules that use C code or the OS to provide the effect of globals are also not permitted.

For example, OCaml's `Unix` module provides access to the network and filesystem to any code that wants it.
By contrast, an Eio module that wants such access must receive it explicitly.

Consider the network example in the previous section.
Imagine this is a large program and we want to know:

1. Does this program modify the filesystem?
2. Does this program send telemetry data over the network?

In an Ocap language, we don't have to read the entire code-base to find the answers:

- All authority starts at the (privileged) `run` function with the `env` parameter,
  so we must check this code.
- Only `env`'s network access is used, so we know this program doesn't access the filesystem,
  answering question 1 immediately.
- To check whether telemetry is sent, we need to follow the `network` authority as it is passed to `main`.
- `main` uses `network` to open a listening socket on the loopback interface, which it passes to `run_server`.
  `run_server` does not get the full `network` access, so we probably don't need to read that code; however,
  we might want to check whether we granted other parties access to this port on our loopback network.
- `run_client` does get `network`, so we do need to read that.
  We could make that code easier to audit by passing it `(fun () -> Eio.Net.connect network addr)` instead of `network`.
  Then we could see that `run_client` could only connect to our loopback address.

Since OCaml is not an Ocap language, code can ignore Eio and use the non-Ocap APIs directly.
Therefore, this cannot be used as a security mechanism.
However, it still makes non-malicious code easier to understand and test
and may allow for an Ocap extension to the language in the future.
See [Emily][] for a previous attempt at this.

## Filesystem Access

Access to the filesystem is also controlled by capabilities, and `env` provides two:

- `fs` provides full access (just like OCaml's stdlib).
- `cwd` restricts access to files beneath the current working directory.

For example:

```ocaml
let try_write_file dir path data =
  match
    Eio.Dir.with_open_out ~create:(`Exclusive 0o600) dir path @@ fun flow ->
    Eio.Flow.copy_string data flow
  with
  | () -> traceln "write %S -> ok" path
  | exception ex -> traceln "write %S -> %a" path Fmt.exn ex

let try_mkdir dir path =
  match Eio.Dir.mkdir dir path ~perm:0o700 with
  | () -> traceln "mkdir %S -> ok" path
  | exception ex -> traceln "mkdir %S -> %a" path Fmt.exn ex
```

```ocaml
# Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  try_mkdir cwd "dir1";
  try_mkdir cwd "../dir2";
  try_mkdir cwd "/tmp/dir3";;
+mkdir "dir1" -> ok
+mkdir "../dir2" -> Eio.Dir.Permission_denied("../dir2", _)
+mkdir "/tmp/dir3" -> Eio.Dir.Permission_denied("/tmp/dir3", _)
- : unit = ()
```

The checks also apply to following symlinks:

```ocaml
# Unix.symlink "dir1" "link-to-dir1"; Unix.symlink "/tmp" "link-to-tmp";;
- : unit = ()

# Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  try_write_file cwd "dir1/file1" "A";
  try_write_file cwd "link-to-dir1/file2" "B";
  try_write_file cwd "link-to-tmp/file3" "C";;
+write "dir1/file1" -> ok
+write "link-to-dir1/file2" -> ok
+write "link-to-tmp/file3" -> Eio.Dir.Permission_denied("link-to-tmp/file3", _)
- : unit = ()
```

You can use `open_dir` (or `with_open_dir`) to create a restricted capability to a subdirectory:

```ocaml
# Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  Eio.Dir.with_open_dir cwd "dir1" @@ fun dir1 ->
  try_write_file dir1 "file4" "D";
  try_write_file dir1 "../file5" "E";;
+write "file4" -> ok
+write "../file5" -> Eio.Dir.Permission_denied("../file5", _)
- : unit = ()
```

You only need to use `open_dir` if you want to create a new sandboxed environment.
You can use a single directory object to access all paths beneath it,
and this allows following symlinks within that subtree.

A program that operates on the current directory will probably want to use `cwd`,
whereas a program that accepts a path from the user will probably want to use `fs`,
perhaps with `open_dir` to constrain all access to be within that directory.

Note: the `eio_luv` backend doesn't have the `openat`, `mkdirat`, etc., calls that are necessary to implement these checks without races.

## Time

The standard environment provides a clock with the usual POSIX time:

```ocaml
# Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  traceln "The time is now %f" (Eio.Time.now clock);
  Eio.Time.sleep clock 1.0;
  traceln "The time is now %f" (Eio.Time.now clock);;
+The time is now 1623940778.270336
+The time is now 1623940779.270336
- : unit = ()
```

You might like to replace this clock with a mock for tests.
In fact, this README does just that! See [doc/prelude.ml](doc/prelude.ml) for the fake clock used in the example above.

## Multicore Support

Fibres are scheduled cooperatively within a single domain, but you can also create new domains that run in parallel.
This is useful to perform CPU-intensive operations quickly.
For example, let's say we have a CPU intensive task:

```ocaml
let sum_to n =
  traceln "Starting CPU-intensive task...";
  let total = ref 0 in
  for i = 1 to n do
    total := !total + i
  done;
  traceln "Finished";
  !total
```

We can use `Eio.Domain_manager` to run this in a separate domain:

```ocaml
let main ~domain_mgr =
  let test n =
    traceln "sum 1..%d = %d" n
      (Eio.Domain_manager.run domain_mgr
        (fun () -> sum_to n))
  in
  Fibre.both
    (fun () -> test 100000)
    (fun () -> test 50000)
```

<!-- $MDX non-deterministic=output -->
```ocaml
# Eio_main.run @@ fun env ->
  main ~domain_mgr:(Eio.Stdenv.domain_mgr env);;
+Starting CPU-intensive task...
+Starting CPU-intensive task...
+Finished
+sum 1..50000 = 1250025000
+Finished
+sum 1..100000 = 5000050000
- : unit = ()
```

Notes:

- `traceln` can be used safely from multiple domains.
  It takes a mutex, so that trace lines are output atomically.
- The exact `traceln` output of this example is non-deterministic,
  because the OS is free to schedule domains as it likes.
- You must ensure that the function passed to `run` doesn't have access to any non-threadsafe values.
  The type system does not check this.
- `run` waits for the domain to finish, but it allows other fibres to run while waiting.
  This is why we use `Fibre.both` to create multiple fibres.

## Design Note: Thread-Safety

OCaml spent the first 25 years of its existence without multicore support, and so most libraries are not thread-safe.
Even in languages that had parallelism from the beginning, thread safety is a very common cause of bugs.
Eio therefore defaults to a conservative model, in which each mutable value is owned and used by a single domain.

There are several ways to share values between domains:

1. Immutable values (such as strings) can always be shared safely.
2. Values specifically designed to be thread-safe (e.g. a `Mutex.t`) can be shared.
3. A non-threadsafe value's owning domain can update it in response to messages from other domains.
4. A non-threadsafe value can be wrapped with a mutex.
5. A non-threadsafe value can passed to another domain if the sending domain will never use it again.

Note that (3) and (4) are not quite the same.
Consider this code:

```ocaml
let example q =
  assert (Queue.length q = Queue.length q)
```

If `q` is only updated by its owning domain (as in 3) then this assertion will always pass.
`Queue.length` will not perform an effect which could switch fibres, so nothing else can update `q`.
If another domain wants to change it, it sends a message to `q`'s domain, which is added to the domain's
run-queue and will take effect later.

However, if `q` is wrapped by a mutex (as in 4) then the assertion could fail.
The first `Queue.length` will lock and then release the queue, then the second will lock and release it again.
Another domain could change the value between these two calls.

## Synchronisation Tools

Eio provides several sub-modules for communicating between fibres and domains.

### Promises

Promises are a simple and reliable way to communicate between fibres.
One fibre can wait for a promise and another can resolve it:

```ocaml
# Eio_main.run @@ fun _ ->
  let promise, resolver = Promise.create () in
  Fibre.both
    (fun () ->
      traceln "Waiting for promise...";
      let x = Promise.await promise in
      traceln "x = %d" x
    )
    (fun () ->
      traceln "Resolving promise";
      Promise.fulfill resolver 42
    );;
+Waiting for promise...
+Resolving promise
+x = 42
- : unit = ()
```

A promise is initially "unresolved". It can then either become "fulfilled" (as in the example above) or "broken" (with an exception).
Either way, the promise is then said to be "resolved". A promise can only be resolved once.
Awaiting a promise that is already resolved immediately returns the resolved value
(or raises the exception, if broken).

Promises are one of the easiest tools to use safely:
it doesn't matter whether you wait on a promise before or after it is resolved,
and multiple fibres can wait for the same promise and will get the same result.
Promises are thread-safe; you can wait for a promise in one domain and resolve it in another.

Promises are also useful for integrating with callback-based libraries. For example:

```ocaml
let wrap fn x =
  let promise, resolver = Promise.create () in
  fn x
    ~on_success:(Promise.resolve resolver)
    ~on_error:(Promise.break resolver);
  Promise.await promise
```

### Streams

A stream is a bounded queue. Reading from an empty stream waits until an item is available.
Writing to a full stream waits for space.

```ocaml
# Eio_main.run @@ fun _ ->
  let stream = Eio.Stream.create 2 in
  Fibre.both
    (fun () ->
       for i = 1 to 5 do
         traceln "Adding %d..." i;
         Eio.Stream.add stream i
       done
    )
    (fun () ->
       for i = 1 to 5 do
         let x = Eio.Stream.take stream in
         traceln "Got %d" x;
         Fibre.yield ()
       done
    );;
+Adding 1...
+Adding 2...
+Adding 3...
+Got 1
+Adding 4...
+Got 2
+Adding 5...
+Got 3
+Got 4
+Got 5
- : unit = ()
```

Here, we create a stream with a maximum size of 2 items.
The first fibre added 1 and 2 to the stream, but had to wait before it could insert 3.

A stream with a capacity of 1 acts like a mailbox.
A stream with a capacity of 0 will wait until both the sender and receiver are ready.

Streams are thread-safe and can be used to communicate between domains.

### Example: a worker pool

A useful pattern is a pool of workers reading from a stream of work items.
Client fibres submit items to a stream and workers process the items:

```ocaml
let handle_job request =
  Fibre.yield ();       (* (simulated work) *)
  Printf.sprintf "Processed:%d" request

let run_worker id stream =
  traceln "Worker %s ready" id;
  while true do
    let request, reply = Eio.Stream.take stream in
    traceln "Worker %s processing request %d" id request;
    Promise.fulfill reply (handle_job request)
  done

let submit stream request =
  let reply, resolve_reply = Promise.create () in
  Eio.Stream.add stream (request, resolve_reply);
  Promise.await reply
```

Each item in the stream is a request payload and a resolver for the reply promise.

```ocaml
# Eio_main.run @@ fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  Switch.run @@ fun sw ->
  let stream = Eio.Stream.create 100 in
  let spawn_worker name =
    Fibre.fork ~sw (fun () ->
       Eio.Domain_manager.run domain_mgr (fun () -> run_worker name stream)
    )
  in
  spawn_worker "A";
  spawn_worker "B";
  Switch.run (fun sw ->
     for i = 1 to 3 do
       Fibre.fork ~sw (fun () ->
         traceln "Client %d submitting job..." i;
         traceln "Client %d got %s" i (submit stream i)
       );
       Fibre.yield ()
     done;
  );
  raise Exit;;
+Worker A ready
+Worker B ready
+Client 1 submitting job...
+Worker A processing request 1
+Client 2 submitting job...
+Worker B processing request 2
+Client 3 submitting job...
+Client 1 got Processed:1
+Worker A processing request 3
+Client 2 got Processed:2
+Client 3 got Processed:3
Exception: Stdlib.Exit.
```

In the code above, any exception raised while processing a job will exit the whole program.
We might prefer to handle exceptions by sending them back to the client and continuing:

```ocaml
let run_worker id stream =
  traceln "Worker %s ready" id;
  while true do
    let request, reply = Eio.Stream.take stream in
    traceln "Worker %s processing request %d" id request;
    match handle_job request with
    | result -> Promise.fulfill reply result
    | exception ex -> Promise.break reply ex; Fibre.check ()
  done
```

The `Fibre.check ()` checks whether the worker itself has been cancelled, and exits the loop if so.
It's not actually necessary in this case,
because if we continue instead then the following `Stream.take` will perform the check anyway.

## Design Note: Determinism

Within a domain, fibres are scheduled deterministically.
Programs using only the Eio APIs can only behave non-deterministically if given a capability to do so from somewhere else.

For example, `Fibre.both f g` always starts running `f` first,
and only switches to `g` when `f` finishes or performs an effect that can switch fibres.

Performing IO with external objects (e.g., `stdout`, files, or network sockets) will introduce non-determinism,
as will using multiple domains.

Note that `traceln` is unusual. Although it writes (by default) to stderr, it will not switch fibres.
Instead, if the OS is not ready to receive trace output, the whole domain is paused until it is ready.
This means that adding `traceln` to deterministic code will not affect its scheduling.

In particular, if you test your code by providing (deterministic) mocks then the tests will be deterministic.
An easy way to write tests is by having the mocks call `traceln` and then comparing the trace output with the expected output.
See Eio's own tests for examples, e.g., [tests/test_switch.md](tests/test_switch.md).

## Examples

- [gemini-eio][] is a simple Gemini browser. It shows how to integrate Eio with `ocaml-tls`, `angstrom`, and `notty`.
- [ocaml-multicore/retro-httpaf-bench](https://github.com/ocaml-multicore/retro-httpaf-bench) includes a simple HTTP server using Eio. It shows how to use Eio with `httpaf`, and how to use multiple domains for increased performance.

## Further Reading

- [lib_eio/eio.mli](lib_eio/eio.mli) documents Eio's public API.
- [doc/rationale.md](doc/rationale.md) describes some of Eio's design tradeoffs in more detail.

Some background about the effects system can be found in:

- [Experiences with effects (video)](https://watch.ocaml.org/videos/watch/74ece0a8-380f-4e2a-bef5-c6bb9092be89), OCaml Workshop 2021.
- ["Retrofitting Concurrency onto OCaml"](https://kcsrk.info/papers/retro-concurrency_pldi_21.pdf) (to appear, PLDI 2021)
- https://kcsrk.info/ocaml/multicore/2015/05/20/effects-multicore/
- Effects examples: https://github.com/ocaml-multicore/effects-examples/tree/master/aio
- [Concurrent System Programming with Effect Handlers](https://www.repository.cam.ac.uk/bitstream/handle/1810/283239/paper.pdf?sequence=3&isAllowed=y)
- [Asynchronous effect based IO using effect handlers](https://github.com/kayceesrk/ocaml-aeio)

[mirage-trace-viewer]: https://github.com/talex5/mirage-trace-viewer
[structured concurrency]: https://en.wikipedia.org/wiki/Structured_concurrency
[typed effects]: https://www.janestreet.com/tech-talks/effective-programming/
[Object-capability model]: https://en.wikipedia.org/wiki/Object-capability_model
[Emily]: https://www.hpl.hp.com/techreports/2006/HPL-2006-116.pdf
[http-bench]: https://github.com/ocaml-multicore/retro-httpaf-bench
[gemini-eio]: https://gitlab.com/talex5/gemini-eio
