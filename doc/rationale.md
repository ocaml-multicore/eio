This document collects some of the reasons behind various API choices in Eio.

## Scheduling Order

When forking a new fiber, there are several reasonable scheduling behaviours:

1. Append the new fiber to the run queue and then continue running the parent.
2. Append the parent fiber to the run queue and start the child immediately.
3. Append both old and new fibers to the run-queue (in some order), then schedule the next task at the queue's head.
4. Prepend the old and new fibers to the *head* of the run-queue and resume one of them immediately.

And several desirable features:

- Especially for `Fiber.both`, putting both at the start or both at the end of the run-queue seems more consistent
  that starting one before everything in the queue and the other after.
- Adding both to the head of the queue is the most flexible, since the other behaviours can then be achieved by yielding.
- Putting both at the head may lead to starvation of other fibers.
- Running the child before the parent allows the child to e.g. create a switch and store it somewhere atomically.
- Scheduling new work to run next can make better use of the cache in some cases (domainslib works this way).

Therefore, `Fiber.fork f` runs `f` immediately and pushes the calling fiber to the head of the run-queue.
After making this change, the examples in the README seemed a bit more natural too.

## Indicating End-of-File

Functions for reading from a byte-stream need a way to indicate that the stream has ended.
There are various common ways to do this:

- Raise the `End_of_file` exception, as `Stdlib.input_line` does.
- Return `None`, as `Stdlib.In_channel.input_line` does.
- Return that 0 bytes were read, as `Stdlib.input` does.
- Return `Error End_of_file`, or similar.

Desirable features:

- A program that forgets to handle end-of-file should not hang.
- The programmer should not forget to handle end-of-file when they need to.
- The programmer should not be forced to handle it when they don't.
- Ideally, reading should not allocate on the heap.
- The meaning of the code should be obvious.

Returning 0 makes infinite loops easy to write.
For example, a function to parse a number from a stream might keep appending to a buffer until it sees a non-digit byte.
If the file ends in the middle of a number, the parser will hang, while working in other cases.

Returning a result or option type forces an allocation even for successful reads.
Mirage's `FLOW` type even uses ``Ok `Eof``, requiring a double allocation on every successful read.
However, it is likely that a read will allocate for other reasons (such as allocating a continuation when performing an effect),
so this is not a serious problem (for unbuffered reads at least).

Returning `None` makes the code unclear - you need to check the documentation to discover whether this applies only to end-of-file or to other kinds of error too, and may encourage programmers to discard error information and return `None` in all cases.

Raising `End_of_file` or returning 0 allows the programmer to forget to handle the error with no compile-time warning.

Eio chooses to use `End_of_file`:

- If you forget to handle it, the cause of the problem is at least obvious (unlike returning 0).
- It makes it easy to handle the error in one place, rather than throughout a parser.
  A backtracking parser is likely to be using exceptions for errors anyway.
- With typed effects it will be possible to track the exception in the type system (unlike returning 0).
- For simple code, with a single read in a loop, you will immediately notice if you don't handle end-of-file.
- For complex code, with multiple reads, you will likely use a parser library that hides this anyway.

## Dynamic Dispatch

Code is easier to understand when the target of a function call is known statically.
However, this is not always possible.
For example, there are many ways to provide a stream of bytes (from a file, TCP socket, HTTP encoding, TLS encoding, etc).
Often this choice is determined by the user at runtime, for example by providing a URL giving the scheme to use.
We may even need to choose a completely different Eio backend at runtime.
For example `Eio_main.run` will use the io_uring backend if the Linux kernel is new enough,
but fall back to `Eio_luv` if not.
For these reasons, Eio needs to use dynamic dispatch.

A resource whose implementation isn't known until runtime can be represented in many ways, including:

- As an object.
- As a record, with one field for each method.
- As a first-class module along with one of its values, packed in a GADT.

OCaml modules have the nice property that they can be used from fully static to fully dynamic situations:

- If a library author knows which concrete module they want,
  they can just call that module directly.

- If the library can be used with different modules,
  but the application using the library will decide which one at compile time,
  the library can use a functor.

- If the module will only be known at runtime, a first-class module can be passed as an argument.

For Eio, we also need good support for sub-typing because different platforms provide different features,
and because different operating system resources have overlapping features.
For example:

- Some flows are read-only, some write-only, and some read-write.
- Most can be closed.
- Some two-way flows support shutting down one side of the connection.
- Some flows are backed by a Unix file descriptor which we may want to extract.

The OCaml standard library provides separate `close_in` and `close_out` functions, but cannot handle two-way flows.
Eio instead provides a single `Flow.close` that works with all flows that can be closed.

Users of Eio can choose how specific to make their code.
For example, calling `Eio_main.run` will get you a basic Unix-like environment,
whereas using `Eio_linux.run` provides extra features specific to Linux's io_uring API.
This can then all be tracked in the type system
(dynamic checks are also possible, for more complex code that wants to use specific features only when available).
In contrast, OCaml's `Unix` module provides several functions that simply fail at runtime
on platforms where the function isn't available.

For dynamic dispatch with subtyping, objects seem to be the best choice:

- Records and modules require explicit casts when used.
  Objects avoid this problem using row-polymorphism.

- Using records or first-class modules requires frequently allocating.
  For example, when you have a `TWO_WAY` module and you want to use it as a `SOURCE`,
  OCaml has to make a copy of the module with the fields in the right order.
  For records, you have to write the code to do this copying yourself.
  Objects don't change their in-memory representation when used at different types.

- Using records means storing all the methods on every instance, which is wasteful.
  Using a GADT adds an extra level of indirection to the value's fields.
  An object uses a single block to store the object's fields and a pointer to the shared method table.

- First-class modules and GADTs are an advanced feature of the language.
  The new users we hope to attract to OCaml 5.00 are likely to be familiar with objects already.

- It is possible to provide base classes with default implementations of some methods.
  This can allow adding new operations to the API in future without breaking existing providers.

In general, simulating objects using other features of the language leads to worse performance
and worse ergonomics than using the language's built-in support.

In Eio, we split the provider and consumer APIs:

- To *provide* a flow, you implement an object type.
- To *use* a flow, you call a function (e.g. `Flow.close`).

The functions mostly just call the corresponding method on the object.
If you call object methods directly in OCaml then you tend to get poor compiler error messages.
This is because OCaml can only refer to the object types by listing the methods you seem to want to use.
Using functions avoids this, because the function signature specifies the type of its argument,
allowing type inference to work as for non-object code.
In this way, users of Eio can be largely unaware that objects are being used at all.

The function wrappers can also provide extra checks that the API is being followed correctly,
such as asserting that a read does not return 0 bytes,
or add extra convenience functions without forcing every implementor to add them too.

Note that the use of objects in Eio is not motivated by the use of the "Object Capabilities" security model.
Despite the name, that is not specific to objects at all.

## Results vs Exceptions

The OCaml standard library uses exceptions to report errors in most cases.
Many libraries instead use the `result` type, which has the advantage of tracking the possible errors in the type system.
However, using `result` is slower, as it requires more allocations, and explicit code to propagate errors.

As part of the effects work, OCaml is expected to gain a [typed effects][] extension to the type system,
allowing it to track both effects and exceptions statically.
In anticipation of this, the Eio library prefers to use exceptions in most cases,
reserving the use of `result` for cases where the caller is likely to want to handle the problem immediately
rather than propagate it.

In additional, while result types work well
for functions with a small number of known errors which can be handled at the call-site,
they work poorly for IO errors where there are typically a large and unknown set of possible errors
(depending on the backend).

[typed effects]: https://www.janestreet.com/tech-talks/effective-programming/
