# Multicore Guide

<!-- vim-markdown-toc GFM -->

* [Introduction](#introduction)
* [Problems with Multicore Programming](#problems-with-multicore-programming)
  * [Optimisation 1: Caching](#optimisation-1-caching)
  * [Optimisation 2: Out-of-Order Execution](#optimisation-2-out-of-order-execution)
  * [Optimisation 3: Compiler Optimisations](#optimisation-3-compiler-optimisations)
  * [Optimisation 4: Multiple Cores](#optimisation-4-multiple-cores)
* [The OCaml Memory Model](#the-ocaml-memory-model)
  * [Atomic Locations](#atomic-locations)
  * [Initialisation](#initialisation)
* [Guidelines](#guidelines)
* [Further Reading](#further-reading)

<!-- vim-markdown-toc -->

## Introduction

OCaml 5.00 adds support for using multiple CPU cores in a single OCaml process.
An OCaml process is made up of one or more *domains*, and
the operating system can run each domain on a different core, so that they run in parallel.
This can make programs run much faster, but also introduces new ways for programs to go wrong.
This guide explains how to write correct multicore programs using Eio.

Note that using multiple cores is only useful to make your program run faster, when one core isn't enough.
Programs that need to juggle a large number of IO tasks
(such as downloading multiple files in parallel while providing an interactive user interface),
but don't need much CPU time, can just use multiple fibers on a single core instead.
Doing that avoids the problems described in this document.

Before we start, we'll define a wrapper around `Eio_main.run` for the examples below.
`run fn` runs an Eio event loop, passing `fn` a function for running things in new domains:

```ocaml
# #require "eio_main";;
# open Eio.Std;;

# let run fn =
    Eio_main.run @@ fun env ->
    let domain_mgr = Eio.Stdenv.domain_mgr env in
    fn (Eio.Domain_manager.run domain_mgr);;
val run : (((unit -> 'a) -> 'a) -> 'b) -> 'b = <fun>
```

## Problems with Multicore Programming

There are two big difficulties with multicore programming.
We'll start with the simpler one.
Consider this program, which runs two fibers on a single domain:

```ocaml
# Eio_main.run @@ fun _ ->
  let x = ref 0 in
  Fiber.both
    (fun () -> incr x)
    (fun () -> incr x);
  traceln "x = %d" !x;;
+x = 2
- : unit = ()
```

Because we're only using a single domain, only one `incr` call can be running at once.
`x` gets incremented twice, and the final result is always `2`.
`both` runs the first fiber until it finishes or waits for something, and then switches to the next runnable fiber.

However, trying to do the same thing with two cores adds a new behaviour:

<!-- $MDX non-deterministic=output -->
```ocaml
# run @@ fun run_in_new_domain ->
  let x = ref 0 in
  Fiber.both
    (fun () -> run_in_new_domain (fun () -> incr x))
    (fun () -> run_in_new_domain (fun () -> incr x));
  traceln "x = %d" !x;;
+x = 1 OR
+x = 2
- : unit = ()
```

The output here may be `x = 1` or `x = 2`.
One way to see that this can happen is to realise that `incr x` is really made up of three steps:

1. Read the current value at location `x`.
2. Add one to it.
3. Store the result back at `x`.

This wasn't a problem when using fibers because none of these steps performs an effect,
so once `incr` starts it continues until it's finished.
But with domains, both domains may perform step 1 at the same time, reading `0`, and
then both domains will write `1` as the result.

It is easy to corrupt more complex data structures in this way.
For example, `Queue.add` increments a length counter and then makes the old last node point to the new one.
If two domains both add an item at the same time, it is possible to end up with a queue with its length counter
set to 2, but only containing one of the added items.

However, this is not the only difficultly with multicore programming.
Consider this program:

```ocaml
let test run_in_new_domain =
  let x = ref 5 in
  let ready = ref false in
  Fiber.both
    (fun () -> run_in_new_domain (fun () ->
       x := !x * 2;
       ready := true
    ))
    (fun () -> run_in_new_domain (fun () ->
       while not !ready do () done;
       traceln "x = %d" !x
    ))
```

The first domain doubles `x` from `5` to `10` and then sets `ready` to `true`.
The second waits until `ready` is `true` and then prints `x`.
The results can be surprising:

<!-- $MDX non-deterministic=output -->
```ocaml
# run test;;
+x = 5  OR
+x = 10 OR
(never finishes)
```

This cannot be explained simply by both domains running at the same time,
which brings us to the second difficulty.
To understand how the above results can happen we need to look at some optimisations computers use to run programs faster.

### Optimisation 1: Caching

A simple model of a computer is a CPU connected to some memory:

```
+-----+         +-----+
| CPU |<------->| RAM |
+-----+         +-----+
```

To perform an operation such as `incr x`, the CPU asks the RAM for the contents of memory address `x`.
It then increments it, and sends the result back to the RAM.

Accessing RAM is relatively slow. A cache can be added to make things faster:

```
+-----+      +-------+      +-----+
| CPU |<---->| Cache |<---->| RAM |
+-----+      +-------+      +-----+
```

When the CPU requests a memory address, the cache fetches it from RAM and remembers what it was.
Next time the CPU wants the same address, the cache can return the remembered value quickly.
The cache can only store values for a limited number of addresses and will forget old addresses as new ones are loaded.

Luckily, you usually don't need to think about the computer's caches.
A program that was correct without a cache is still correct with it; it just runs faster.

### Optimisation 2: Out-of-Order Execution

Consider the code `incr x; incr y`.
If executed in order, the CPU will:

1. Request `x` from the memory.
2. Wait for it to arrive.
3. Add one and write it back.
4. Request `y` from the memory.
5. Wait for it to arrive.
6. Add one and write it back.

This is very slow. When a modern CPU gets to step 2, it will look for other things it could be doing while waiting.
It can't do steps 2 or 3 (they depend on the fetched value), but it can start fetching `y`:

1. Request `x` from the memory.
2. Request `y` from the memory.
3. Wait for `x` to arrive.
4. Add one and write it back.
5. Wait for `y` to arrive.
6. Add one and write it back.

This can go much faster.

Luckily, you usually don't need to think about out-of-order execution.
The CPU will only change the order when it won't make any visible difference, except to make the program run faster.

### Optimisation 3: Compiler Optimisations

Consider the code:

```ocaml
let debug = ref false

let foo x =
  if !debug then print_endline "enter";
  incr x;
  if !debug then print_endline "leave"
```

It would be wasteful to load `debug` from the memory twice.
Since `foo` doesn't change it after reading, the compiler can just reuse the previous load,
as if we'd written:

```ocaml
let debug = ref false

let foo x =
  let d = !debug in
  if d then print_endline "enter";
  incr x;
  if d then print_endline "leave"
```

This is even faster than using the cache.
Luckily, you usually don't need to think about compiler optimisations.
The compiler will only optimise code if it doesn't change the program's behaviour, except to make it faster.

### Optimisation 4: Multiple Cores

We can make a computer faster by having multiple CPUs (or cores within a CPU):

```
+-------+      +-------+      +-----+
| CPU-A |<---->| Cache |<---->|     |
+-------+      +-------+      |     |
                              | RAM |
+-------+      +-------+      |     |
| CPU-B |<---->| Cache |<---->|     |
+-------+      +-------+      +-----+
```

Unluckily, this makes all of the previous optimisations unsound.

Recall the test program above:

```ocaml
let test run_in_new_domain =
  let x = ref 5 in
  let ready = ref false in
  Fiber.both
    (fun () -> run_in_new_domain (fun () ->
       x := !x * 2;
       ready := true
    ))
    (fun () -> run_in_new_domain (fun () ->
       while not !ready do () done;
       traceln "x = %d" !x
    ))
```

If the first branch runs on CPU-A and the second on CPU-B:

1. The first iteration of the `while` loop will load `ready=false` into CPU-B's cache.
   It might then just keep using this cached value forever, even after CPU-A has updated it in the main memory.

2. While CPU-A is waiting to read the old value of `x`, it might get on with setting `ready := true`.
   The while loop might then finish and print `x = 5` before CPU-A has written the new value for `x`.

3. The compiler might notice that `ready` isn't changed in the `while` loop.
   It might optimise it to: `let r = !ready in while r do () done ...` and never finish.

How can we write a correct program when our caches, CPUs and compiler are changing our program's behaviour?
The solution is to use a *memory model*.

## The OCaml Memory Model

The [OCaml Memory Model][] defines an imaginary computer system, and guarantees that a real system will not do anything that one wouldn't.
If your program would run correctly on the imaginary system, it will run correctly on a real one too.
The imaginary system has no caches, no out-of-order execution, and no compiler optimisations.
It does, however, have a slightly odd kind of memory.

Each memory location has a *timeline* of values, rather than storing a single value like a normal memory location,
and each domain has a position on each timeline.
When our `test` program above has created the two `ref` cells and is about to spawn the domains, the memory looks like this:

```
x     : [AB]5----------------->
ready : [AB]false------------->
```

This means that e.g. `ready`'s timeline contains just the initial value `false`,
and both processors are at that point on the timeline.
The set of positions over all timelines is called the CPU's *frontier*.
For example, the column of `A`s above is `A`'s frontier.

When a processor reads from a memory location, it may get back any value at or after its current position.
At the moment, there is only one value on each timeline, so reads will just return that.

When a processor writes to a memory location, it adds the new value somewhere to the right of its current position,
and then moves to that position.
So once the first branch has performed the `x := !x * 2` step, we have:

```
x     : [B]5--------[A]10----->
ready : [AB]false------------->
```

If `A` reads from `x` at this point it will certainly read back the `10` it just wrote.
But if `B` were to read from `x`, it might see `5` or it might see `10`.
Concretely, this corresponds to possibilities such as `10` being in the computer's main RAM
but `5` being in `B`'s cache.

The first branch then sets `ready := true`:

```
x     : [B]5--------[A]10----->
ready : [B]false---[A]true---->
```

I've shown the `true` slightly left of the `10` just to emphasise that the timelines are independent;
a write can go anywhere to the right of the current location.

If the `while` loop keeps reading `false` (which it can) then the program will not terminate.
If it does read `true`, it will then read and display `x`, which it might see as either `5` or `10`.
This explains the three possible behaviours of the program on a real computer,
without having to think about caches, out-of-order execution, or compiler optimisations.

### Atomic Locations

OCaml also provides special "atomic" locations.
An atomic location just stores a single value; it does not have a timeline.
We could fix our program by replacing the `ref` cells with atomics, like this:

```ocaml
let test run_in_new_domain =
  let x = Atomic.make 5 in
  let ready = Atomic.make false in
  Fiber.both
    (fun () -> run_in_new_domain (fun () ->
       Atomic.set x (Atomic.get x * 2);
       Atomic.set ready true
    ))
    (fun () -> run_in_new_domain (fun () ->
       while not (Atomic.get ready) do () done;
       traceln "x = %d" (Atomic.get x)
    ))
```

This version will always produce the expected result:

```ocaml
# run test;;
+x = 10
- : unit = ()
```

However, using atomics everywhere is slow.
For example, the first `Atomic.get x` might require loading `x` from RAM, even if it was already in the cache.

We can solve this by relying on a useful feature of atomics:
every atomic also has a frontier of its own (a location on every non-atomic location's timeline).
The union of two frontiers is a frontier where each timeline point is the maximum of the two inputs.
Writing to an atomic sets both the writer's frontier and the atomic's frontier to the union of them both.
Reading from an atomic is similar, but updates only the reader's frontier.

Here's a new version, mixing atomic and non-atomic locations:

```ocaml
let test run_in_new_domain =
  let x = ref 5 in
  let ready = Atomic.make false in
  Fiber.both
    (fun () -> run_in_new_domain (fun () ->
       x := !x * 2;
       Atomic.set ready true
    ))
    (fun () -> run_in_new_domain (fun () ->
       while not (Atomic.get ready) do () done;
       traceln "x = %d" !x
    ))
```

Initially (before the `Fiber.both`) the memory looks like this:

```
x     : [ABR]5----------------->
ready : false   (atomic)
```

Notice that `ready` now has a frontier too, marked as `R`.
After writing `x`, we have:

```
x     : [BR]5--------[A]10----->
ready : false   (atomic)
```

When the first branch writes to the atomic location `ready`, `[R]` gets updated too:

```
x     : [B]5--------[AR]10----->
ready : true   (atomic)
```

At this point, if `B` were to read `x` it might see `5` or `10`, but after reading from `ready` it not only learns that its value is now `true`,
but also gets its frontier updated with information from `R`:

```
x     : 5--------[ABR]10----->
ready : true   (atomic)
```

Therefore, this version will always print `10`:

```ocaml
# run test;;
+x = 10
- : unit = ()
```

Of course, atomic locations do not store frontiers in a real computer.
It's just a way of thinking about what values can be read after accessing an atomic location.
For example, a real system must not perform the write to `ready` before issuing the write to `x`.

### Initialisation

In the above examples, the locations were set up before spawning the new domains.
What if we create new locations from a domain? For example, what does this do?

```ocaml
let test run_in_new_domain =
  let x = ref [1; 2; 3] in
  Fiber.both
    (fun () -> run_in_new_domain (fun () ->
       x := [4; 5; 6]
    ))
    (fun () -> run_in_new_domain (fun () ->
       traceln "x = %a" Fmt.(Dump.list int) !x
    ))
```

If the second branch sees the old value of `x` then it will print `[1; 2; 3]`.
But if it sees the new value (a pointer to a newly allocated list), can we be sure those locations are initialised?
The answer is yes: a location's timeline in the OCaml memory model starts when the location is allocated
(it is not a physical memory address which might get reused).
In particular, an immutable value only ever has one item on its timeline, and so you can only ever read that value.
So it will always see a correct list:

<!-- $MDX non-deterministic=output -->
```ocaml
# run test;;
+x = [1; 2; 3] OR
+x = [4; 5; 6]
- : unit = ()
```

## Guidelines

It's important to understand the above to avoid writing incorrect code,
but there are several general principles that avoid most problems:

- Immutable values can always be shared safely between domains.

- To transfer mutable data, the sending domain must write to an atomic location after making the changes,
  and the receiver must read from the same atomic location before reading the data.
  The sender must not mutate the data after sending it.

- Higher-level thread-safe primitives use atomics internally.
  For example, protecting mutable data with a `Mutex` (only accessing the data while holding the mutex)
  will ensure that all domains always see the latest writes.
  Likewise, resolving an Eio `Promise` or adding data to an Eio `Stream` will write to an atomic location,
  and reading the promise or taking the value from the stream will read the atomic,
  ensuring that the receiver is up-to-date with the state of the data (assuming it wasn't
  modified afterwards).

- OCaml spent the first 25 years of its existence without multicore support, and so most libraries are not thread-safe.
  Even in languages that had parallelism from the beginning, thread safety is a very common cause of bugs.
  You should assume that values created by an OCaml library are not multicore safe, unless specified otherwise.

There are several reasonable ways to share mutable values between domains:

1. Values specifically designed to be thread-safe (e.g. a `Mutex.t`) can be shared.
2. A non-threadsafe value's owning domain can update it in response to messages from other domains.
3. A non-threadsafe value can be wrapped with a mutex.
4. A non-threadsafe value can passed to another domain if the sending domain will never use it again.

Note that (2) and (3) are not quite the same.
Consider this code:

```ocaml
let example q =
  assert (Queue.length q = Queue.length q)
```

If `q` is only updated by its owning domain (as in 2) then this assertion will always pass.
`Queue.length` will not perform an effect which could switch fibers, so nothing else can update `q`.
If another domain wants to change it, it sends a message to `q`'s domain, which is added to the domain's
run-queue and will take effect later.

However, if `q` is wrapped by a mutex (as in 3) then the assertion could fail.
The first `Queue.length` will lock and then release the queue, then the second will lock and release it again.
Another domain could change the value between these two calls.

You can often run most of your program's logic in a single domain, using fibers,
while sending self-contained CPU-intensive jobs to a pool of worker domains.
This gets most of the benefits of using multiple domains while avoiding most of the problems.
See the "Worker Pool" example in the main README for an example.

Finally, note that OCaml remains type-safe even with multiple domains.
For example, accessing a `Queue` in parallel from multiple domains may result in a corrupted queue,
but it won't cause a segfault.

## Further Reading

- [OCaml Memory Model][] describes the full details of the memory model.
- [Separation Logic Foundations][] introduces Separation Logic, which allows specifying code in a composable way.
  It is particularly useful for reasoning about parallel systems.
- [Parallel Programming in Multicore OCaml][] provides help for writing high-performance multicore code in OCaml.

[OCaml Memory Model]: https://kcsrk.info/papers/pldi18-memory.pdf
[Separation Logic Foundations]: https://softwarefoundations.cis.upenn.edu/slf-current/index.html
[Parallel Programming in Multicore OCaml]: https://github.com/ocaml-multicore/parallel-programming-in-multicore-ocaml
