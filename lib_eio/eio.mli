(** Effects based parallel IO for OCaml.

    Eio provides support for concurrency (juggling many tasks) and
    parallelism (using multiple CPU cores for performance).

    It provides facilities for creating and coordinating fibers (light-weight
    threads) and domains (for parallel processing), as well as interfaces for
    interacting with resources provided by the operating system.

    These features must be used within an {e event loop},
    provided by an Eio {e backend}.
    Applications can use {!Eio_main.run} to run a suitable loop.

    See {{:https://github.com/ocaml-multicore/eio}} for a tutorial. *)

(** {1 Concurrency primitives} *)

(** Grouping fibers and other resources so they can be turned off together. *)
module Switch : sig
  (** Many resources in Eio (such as fibers and file handles) require a switch to
      be provided when they are created. The resource cannot outlive its switch.

      If a function wants to create such resources, and was not passed a switch
      as an argument, it will need to create a switch using {!run}.
      This doesn't return until all resources attached to it have been freed,
      preventing the function from leaking resources.

      Any function creating resources that outlive it needs to be given a
      switch by its caller.

      Each switch includes its own {!Cancel.t} context.
      Calling {!fail} cancels all fibers attached to the switch and, once they
      have exited, reports the error.

      Note: this concept is known as a "nursery" or "bundle" in some other systems.

      Example:
      {[
         Switch.run (fun sw ->
            let flow = Dir.open_in ~sw dir "myfile.txt" in
            ...
         );
         (* [flow] will have been closed by this point *)
      ]}
  *)

  type t
  (** A switch contains a group of fibers and other resources (such as open file handles). *)

  (** {2 Switch creation} *)

  val run : (t -> 'a) -> 'a
  (** [run fn] runs [fn] with a fresh switch (initially on).

      When [fn] finishes, [run] waits for all fibers registered with the switch to finish,
      and then releases all attached resources.

      If {!fail} is called, [run] will re-raise the exception (after everything is cleaned up).
      If [fn] raises an exception, it is passed to {!fail}. *)

  val run_protected : (t -> 'a) -> 'a
  (** [run_protected fn] is like [run] but ignores cancellation requests from the parent context. *)

  (** {2 Cancellation and failure} *)

  val check : t -> unit
  (** [check t] checks that [t] is still on.
      @raise Cancel.Cancelled If the switch has been cancelled. *)

  val get_error : t -> exn option
  (** [get_error t] is like [check t] except that it returns the exception instead of raising it.
      If [t] is finished, this returns (rather than raising) the [Invalid_argument] exception too. *)

  val fail : ?bt:Printexc.raw_backtrace -> t -> exn -> unit
  (** [fail t ex] adds [ex] to [t]'s set of failures and
      ensures that the switch's cancellation context is cancelled,
      to encourage all fibers to exit as soon as possible.

      [fail] returns immediately, without waiting for the shutdown actions to complete.
      The exception will be raised later by {!run}, and [run]'s caller is responsible for handling it.
      {!Exn.combine} is used to avoid duplicate or unnecessary exceptions.
      @param bt A backtrace to attach to [ex] *)

  (** {2 Cleaning up resources}

      It is possible to attach clean-up hooks to a switch.
      Once all fibres within the switch have finished, these hooks are called.
      For example, when a file is opened it will register a release hook to close it.

      Functions that create such resources will take a switch argument
      and call these functions for you.
      You usually don't need to call these directly. *)

  val on_release : t -> (unit -> unit) -> unit
  (** [on_release t fn] registers [fn] to be called once [t]'s main function has returned
      and all fibers have finished.

      If [fn] raises an exception, it is passed to {!fail}.

      Release handlers are run in LIFO order, in series.

      Note that [fn] is called within a {!Cancel.protect}, since aborting clean-up actions is usually a bad idea
      and the switch may have been cancelled by the time it runs. *)

  type hook
  (** A handle for removing a clean-up callback. *)

  val null_hook : hook
  (** A dummy hook. Removing it does nothing. *)

  val on_release_cancellable : t -> (unit -> unit) -> hook
  (** Like [on_release], but the handler can be removed later.

      For example, opening a file will call [on_release_cancellable] to ensure the file is closed later.
      However, if the file is manually closed before that, it will use {!remove_hook} to remove the hook,
      which is no longer needed. *)

  val remove_hook : hook -> unit
  (** [remove_hook h] removes a previously-added hook.
      If the hook has already been removed, this does nothing. *)

  (** {2 Debugging} *)

  val dump : t Fmt.t
  (** Dump out details of the switch's state for debugging. *)
end

(** A promise is a placeholder for result that will arrive in the future. *)
module Promise : sig
  (** Unlike lazy values, you cannot "force" promises;
      a promise is resolved when the maker of the promise is ready.

      Promises are thread-safe and so can be shared between domains and used
      to communicate between them.

      Example:
      {[
        let promise, resolver = Promise.create () in
        Fiber.both
          (fun () -> traceln "Got %d" (Promise.await promise))
          (fun () -> Promise.resolve resolver 42)
      ]} *)

  type !'a t
  (** An ['a t] is a promise for a value of type ['a]. *)

  type 'a u
  (** An ['a u] is a resolver for a promise of type ['a]. *)

  val create : ?label:string -> unit -> 'a t * 'a u
  (** [create ()] is a fresh promise/resolver pair.
      The promise is initially unresolved. *)

  val create_resolved : 'a -> 'a t
  (** [create_resolved x] is a promise that is already resolved with result [x]. *)

  val await : 'a t -> 'a
  (** [await t] blocks until [t] is resolved.
      If [t] is already resolved then this returns immediately. *)

  val resolve : 'a u -> 'a -> unit
  (** [resolve u v] resolves [u]'s promise with the value [v].
      Any threads waiting for the result will be added to the run queue.
      @raise Invalid_argument if [u] is already resolved. *)

  val peek : 'a t -> 'a option
  (** [peek t] is [Some v] if the promise has been resolved to [v], or [None] otherwise.
      If the result is [None] then it may change in future, otherwise it won't.
      If another domain has access to the resolver then the state may have already
      changed by the time this call returns. *)

  val is_resolved : 'a t -> bool
  (** [is_resolved t] is [Option.is_some (peek t)]. *)

  (** {1 Result promises} *)

  type 'a or_exn = ('a, exn) result t

  val resolve_ok : ('a, 'b) result u -> 'a -> unit
  (** [resolve_ok u x] is [resolve u (Ok x)]. *)

  val resolve_error : ('a, 'b) result u -> 'b -> unit
  (** [resolve_error u x] is [resolve u (Error x)]. *)

  val await_exn : 'a or_exn -> 'a
  (** [await_exn t] is like [await t], but if the result is [Error ex] then it raises [ex]. *)
end

(** A fiber is a light-weight thread. *)
module Fiber : sig
  (** Within a domain, only one fiber can be running at a time.
      A fiber runs until it performs an IO operation (directly or indirectly).
      At that point, it may be suspended and the next fiber on the run queue runs. *)

  val both : (unit -> unit) -> (unit -> unit) -> unit
  (** [both f g] runs [f ()] and [g ()] concurrently.

      They run in a new cancellation sub-context, and
      if either raises an exception, the other is cancelled.
      [both] waits for both functions to finish even if one raises
      (it will then re-raise the original exception).

      [f] runs immediately, without switching to any other thread.
      [g] is inserted at the head of the run-queue, so it runs next even if other threads are already enqueued.
      You can get other scheduling orders by adding calls to {!yield} in various places.
      e.g. to append both fibers to the end of the run-queue, yield immediately before calling [both].

      If both fibers fail, {!Exn.combine} is used to combine the exceptions. *)

  val pair : (unit -> 'a) -> (unit -> 'b) -> 'a * 'b
  (** [pair f g] is like [both], but returns the two results. *)

  val all : (unit -> unit) list -> unit
  (** [all fs] is like [both], but for any number of fibers.
      [all []] returns immediately. *)

  val first : (unit -> 'a) -> (unit -> 'a) -> 'a
  (** [first f g] runs [f ()] and [g ()] concurrently.

      They run in a new cancellation sub-context, and when one finishes the other is cancelled.
      If one raises, the other is cancelled and the exception is reported.

      As with [both], [f] runs immediately and [g] is scheduled next, ahead of any other queued work.

      If both fibers fail, {!Exn.combine} is used to combine the exceptions. *)

  val any : (unit -> 'a) list -> 'a
  (** [any fs] is like [first], but for any number of fibers.

      [any []] just waits forever (or until cancelled). *)

  val await_cancel : unit -> 'a
  (** [await_cancel ()] waits until cancelled.
      @raise Cancel.Cancelled *)

  val fork : sw:Switch.t -> (unit -> unit) -> unit
  (** [fork ~sw fn] runs [fn ()] in a new fiber, but does not wait for it to complete.

      The new fiber is attached to [sw] (which can't finish until the fiber ends).

      The new fiber inherits [sw]'s cancellation context.
      If the fiber raises an exception, [Switch.fail sw] is called.
      If [sw] is already off then [fn] fails immediately, but the calling thread continues.

      [fn] runs immediately, without switching to any other fiber first.
      The calling fiber is placed at the head of the run queue, ahead of any previous items. *)

  val fork_sub : sw:Switch.t -> on_error:(exn -> unit) -> (Switch.t -> unit) -> unit
  (** [fork_sub ~sw ~on_error fn] is like [fork], but it creates a new sub-switch for the fiber.

      This means that you can cancel the child switch without cancelling the parent.
      This is a convenience function for running {!Switch.run} inside a {!fork}.

      @param on_error This is called if the fiber raises an exception.
                      If it raises in turn, the parent switch is failed.
                      It is not called if the parent [sw] itself is cancelled. *)

  val fork_on_accept :
    on_handler_error:(exn -> unit) ->
    sw:Switch.t ->
    (Switch.t -> 'a) ->
    (Switch.t -> 'a -> unit) ->
    unit
  (** [fork_on_accept ~sw accept handle ~on_handler_error] creates a new sub-switch [t].
      It runs [accept t] in the current fiber and, on success, runs [handle t result] in a new fiber.
      It is useful for e.g. accepting network connections,
      where we need to provide a switch for the new client socket before we have forked,
      but then move it to a child fiber later.

      If [accept] raises an exception then the effect is the same as [Switch.run accept].
      If [handle] raises an exception, it is passed to [on_handler_error].
      If that raises in turn, the parent switch is failed.
      [on_handler_error] is not called if the parent [sw] is itself cancelled. *)

  val fork_promise : sw:Switch.t -> (unit -> 'a) -> 'a Promise.or_exn
  (** [fork_promise ~sw fn] schedules [fn ()] to run in a new fiber and returns a promise for its result.

      This is just a convenience wrapper around {!fork}.
      If [fn] raises an exception then the promise is resolved to the error, but [sw] is not failed. *)

  val check : unit -> unit
  (** [check ()] checks that the fiber's context hasn't been cancelled.
      Many operations automatically check this before starting.
      @raise Cancel.Cancelled if the fiber's context has been cancelled. *)

  val yield : unit -> unit
  (** [yield ()] asks the scheduler to switch to the next runnable task.
      The current task remains runnable, but goes to the back of the queue.
      Automatically calls {!check} just before resuming. *)
end

(**/**)
module Fibre = Fiber [@@deprecated "Now spelt Fiber"]
(**/**)

(** A counting semaphore. *)
module Semaphore : sig
  (** The API is based on OCaml's [Semaphore.Counting].

      The difference is that when waiting for the semaphore this will switch to the next runnable fiber,
      whereas the stdlib one will block the whole domain.

      Semaphores are thread-safe and so can be shared between domains and used
      to synchronise between them. *)

  type t
  (** The type of counting semaphores. *)

  val make : int -> t
  (** [make n] returns a new counting semaphore, with initial value [n].
      The initial value [n] must be nonnegative.
      @raise Invalid_argument if [n < 0] *)

  val release : t -> unit
  (** [release t] increments the value of semaphore [t].
      If other fibers are waiting on [t], the one that has been waiting the longest is resumed.
      @raise Sys_error if the value of the semaphore would overflow [max_int] *)

  val acquire : t -> unit
  (** [acquire t] blocks the calling fiber until the value of semaphore [t]
      is not zero, then atomically decrements the value of [t] and returns. *)

  val get_value : t -> int
  (** [get_value t] returns the current value of semaphore [t]. *)
end

(** Mutual exclusion. *)
module Mutex : sig 
  (** A mutex can be used to ensure that only one piece of code can access a shared resource at one time.

      Unlike {!Stdlib.Mutex}, which blocks the whole domain while waiting to take the mutex,
      this module allows other Eio fibers to run while waiting.
      You should use this module if your critical section may perform blocking operations,
      while [Stdlib.Mutex] may be more efficient if the lock is held only briefly and
      the critial section does not switch fibers.

      Note that mutexes are often unnecessary for code running in a single domain, as
      the scheduler will only switch to another fiber if you perform an operation that
      can block. *)
  
  type t
  (** The type for a concurrency-friendly mutex. *)

  exception Poisoned of exn
  (** Raised if you attempt to use a mutex that has been disabled. *)
  
  val create : unit -> t
  (** [create ()] creates an initially unlocked mutex. *)

  val use_rw : protect:bool -> t -> (unit -> 'a) -> 'a
  (** [use_rw ~protect t fn] waits for the mutex to be free and then executes [fn ()] while holding the mutex locked.
      [fn] may mutate the resource protected by the mutex,
      but must ensure the resource is in a consistent state before returning.
      If [fn] raises an exception, the mutex is disabled and cannot be used again.
      @param protect If [true], uses {!Cancel.protect} to prevent the critical section from being cancelled.
                     Cancellation is not prevented while waiting to take the lock. *)

  val use_ro : t -> (unit -> 'a) -> 'a
  (** [use_ro t fn] is like {!use_rw ~protect:false},
      but if [fn] raises an exception it unlocks the mutex instead of disabling it.
      Use this if you only need read-only access to the mutex's resource and so
      know that it will be in a consistent state even if an exception is raised. *)

  (** {2 Low-level API}

      Care must be taken when locking a mutex manually. It is easy to forget to unlock it in some cases,
      which will result in deadlock the next time a fiber tries to use it.
      In particular, you need to consider:

      - What happens if your critical section raises an exception.
      - What happens if your fiber is cancelled while in its critical section.
   *)
  
  val lock : t -> unit
  (** Lock the given mutex. Only one fiber can have the mutex locked at any time.
      A fiber that attempts to lock a mutex already locked by another fiber
      will suspend until the other fiber unlocks the mutex.
      If no other fiber has the lock, this returns immediately without switching fibers. *)
  
  val unlock : t -> unit
  (** [unlock t] unlocks the mutex.
      @raises Sys_error if the mutex is unlocked. *)

  val try_lock : t -> bool
  (** Same as {!lock}, but does not suspend the calling thread if the mutex is already locked:
      just return [false] immediately in that case. If the mutex is unlocked, lock it and return [true]. *)
end

(** A stream/queue. *)
module Stream : sig
  (** Reading from an empty queue will wait until an item is available.
      Writing to a full queue will wait until there is space.

      Example:
      {[
        let t = Stream.create 100 in
        Stream.add t 1;
        Stream.add t 2;
        assert (Stream.take t = 1);
        assert (Stream.take t = 2)
      ]}

      Streams are thread-safe and so can be shared between domains and used
      to communicate between them. *)

  type 'a t
  (** A queue of items of type ['a]. *)

  val create : int -> 'a t
  (** [create capacity] is a new stream which can hold up to [capacity] items without blocking writers.

      - If [capacity = 0] then writes block until a reader is ready.
      - If [capacity = 1] then this acts as a "mailbox".
      - If [capacity = max_int] then the stream is effectively unbounded. *)

  val add : 'a t -> 'a -> unit
  (** [add t item] adds [item] to [t].

      If this would take [t] over capacity, it blocks until there is space. *)

  val take : 'a t -> 'a
  (** [take t] takes the next item from the head of [t].

      If no items are available, it waits until one becomes available. *)

  val take_nonblocking : 'a t -> 'a option
  (** [take_nonblocking t] is like [Some (take t)] except that
      it returns [None] if the stream is empty rather than waiting.

      Note that if another domain may add to the stream then a [None]
      result may already be out-of-date by the time this returns. *)

  val length : 'a t -> int
  (** [length t] returns the number of items currently in [t]. *)

  val is_empty : 'a t -> bool
  (** [is_empty t] is [length t = 0]. *)
end

(** Cancelling fibers. *)
module Cancel : sig
  (** This is the low-level interface to cancellation.
      Every {!Switch} includes a cancellation context and most users will just use that API instead.

      Each domain has a tree of cancellation contexts, and every fiber is registered with one context.
      A fiber can switch to a different context (e.g. by calling {!sub}).
      When a context is cancelled, all registered fibers have their current cancellation function (if any)
      called and removed. Child contexts are cancelled too, recursively, unless marked as protected.

      Many operations also check that the current context hasn't been cancelled,
      so if a fiber is performing a non-cancellable operation it will still get cancelled soon afterwards.
      This check is typically done when starting an operation, not at the end.
      If an operation is cancelled after succeeding, but while still waiting on the run queue,
      it will still return the operation's result.
      A notable exception is {!Fiber.yield}, which checks at the end.
      You can also use {!Fiber.check} to check manually.

      Whether a fiber is cancelled through a cancellation function or by checking its context,
      it will receive a {!Cancelled} exception.
      It is possible the exception will get lost (if something catches it and forgets to re-raise).
      It is also possible to get this exception even when not cancelled, for example by awaiting
      a promise which another fiber has resolved to a cancelled exception.
      When in doubt, use [Fiber.check ()] to find out if your fiber is really cancelled.
      Ideally this should be done any time you have caught an exception and are planning to ignore it,
      although if you forget then the next IO operation will typically abort anyway.

      Quick clean-up actions (such as releasing a mutex or deleting a temporary file) are OK,
      but operations that may block should be avoided.
      For example, a network connection should simply be closed,
      without attempting to send a goodbye message.

      The purpose of the cancellation system is to stop fibers quickly, not to report errors.
      Use {!Switch.fail} instead to record an error. *)

  type t
  (** A cancellation context. *)

  exception Cancelled of exn
  (** [Cancelled ex] indicates that the context was cancelled with exception [ex].
      It is usually not necessary to report a [Cancelled] exception to the user,
      as the original problem will be handled elsewhere.

      The nested exception is only intended for debug-level logging and should generally be ignored. *)

  exception Cancel_hook_failed of exn list
  (** Raised by {!cancel} if any of the cancellation hooks themselves fail. *)

  val sub : (t -> 'a) -> 'a
  (** [sub fn] installs a new cancellation context [t], runs [fn t] inside it, and then restores the old context.

      If the old context is cancelled while [fn] is running then [t] is cancelled too.
      [t] cannot be used after [sub] returns. *)

  val protect : (unit -> 'a) -> 'a
  (** [protect fn] runs [fn] in a new cancellation context that isn't cancelled when its parent is.

      This can be used to clean up resources on cancellation.
      However, it is usually better to use {!Switch.on_release} (which calls this for you).

      Note that [protect] does not check its parent context when it finishes. *)

  val check : t -> unit
  (** [check t] checks that [t] hasn't been cancelled.
      @raise Cancelled If the context has been cancelled. *)

  val get_error : t -> exn option
  (** [get_error t] is like [check t] except that it returns the exception instead of raising it.

      If [t] is finished, this returns (rather than raising) the [Invalid_argument] exception too. *)

  val cancel : t -> exn -> unit
  (** [cancel t ex] marks [t] and its child contexts as cancelled, recursively,
      and calls all registered fibers' cancellation functions, passing [Cancelled ex] as the argument.

      All cancellation functions are run, even if some of them raise exceptions.

      If [t] is already cancelled then this does nothing.

      Note that the caller of this function is still responsible for handling the error somehow
      (e.g. reporting it to the user); it does not become the responsibility of the cancelled thread(s).

      @raise Cancel_hook_failed if one or more hooks fail. *)

  val dump : t Fmt.t
  (** Show the cancellation sub-tree rooted at [t], for debugging. *)
end

(** Commonly used standard features. This module is intended to be [open]ed. *)
module Std : sig
  module Promise = Promise
  module Fiber = Fiber
  (**/**)
  module Fibre = Fiber [@@deprecated "Now spelt Fiber"]
  (**/**)
  module Switch = Switch

  val traceln :
    ?__POS__:string * int * int * int ->
    ('a, Format.formatter, unit, unit) format4 -> 'a
    (** Same as {!Eio.traceln}. *)
end

(** {1 Cross-platform OS API}

    The general pattern here is that each type of resource has a set of functions for using it,
    plus an object type to allow defining your own implementations.
    To use the resources, it is recommended that you use the functions rather than calling
    methods directly. Using the functions results in better error messages from the compiler,
    and may provide extra features or sanity checks.

    The system resources are available from the {!Stdenv.t} provided by your event loop
    (e.g. {!Lwt_main.run}). *)

(** A base class for objects that can be queried at runtime for extra features. *)
module Generic : sig
  type 'a ty = ..
  (** An ['a ty] is a query for a feature of type ['a]. *)

  class type t = object
    method probe : 'a. 'a ty -> 'a option
  end

  val probe : #t -> 'a ty -> 'a option
  (** [probe t feature] checks whether [t] supports [feature].
      This is mostly for internal use.
      For example, {!Eio_unix.FD.peek} uses this to get the underlying Unix file descriptor from a flow. *)
end

(** Byte streams. *)
module Flow : sig
  (** Flows are used to represent byte streams, such as open files and network sockets.
      A {!source} provides a stream of bytes. A {!sink} consumes a stream.
      A {!two_way} can do both.

      To read structured data (e.g. a line at a time), wrap a source using {!Buf_read}. *)

  (** {2 Reading} *)

  type read_method = ..
  (** Sources can offer a list of ways to read them, in order of preference. *)

  class virtual source : object
    inherit Generic.t
    method read_methods : read_method list
    method virtual read_into : Cstruct.t -> int
  end

  val read : #source -> Cstruct.t -> int
  (** [read src buf] reads one or more bytes into [buf].

      It returns the number of bytes read (which may be less than the
      buffer size even if there is more data to be read).
      [read src] just makes a single call to [src#read_into]
      (and asserts that the result is in range).

      - Use {!read_exact} instead if you want to fill [buf] completely.
      - Use {!Buf_read.line} to read complete lines.
      - Use {!copy} to stream data directly from a source to a sink.

      [buf] must not be zero-length.

      @raise End_of_file if there is no more data to read *)

  val read_exact : #source -> Cstruct.t -> unit
  (** [read_exact src dst] keeps reading into [dst] until it is full.
      @raise End_of_file if the buffer could not be filled. *)

  val read_methods : #source -> read_method list
  (** [read_methods flow] is a list of extra ways of reading from [flow],
      with the preferred (most efficient) methods first.

      If no method is suitable, {!read} should be used as the fallback. *)

  val string_source : string -> source
  (** [string_source s] is a source that gives the bytes of [s]. *)

  val cstruct_source : Cstruct.t list -> source
  (** [cstruct_source cs] is a source that gives the bytes of [cs]. *)

  type read_method += Read_source_buffer of ((Cstruct.t list -> unit) -> unit)
  (** If a source offers [Read_source_buffer rsb] then the user can call [rsb fn]
      to borrow a view of the source's buffers.

      [rsb] will raise [End_of_file] if no more data will be produced.
      If no data is currently available, [rsb] will wait for some to become available before calling [fn].

      [fn] must not continue to use the buffers after it returns. *)

  (** {2 Writing} *)

  (** Consumer base class. *)
  class virtual sink : object
    inherit Generic.t
    method virtual copy : 'a. (#source as 'a) -> unit
  end

  val copy : #source -> #sink -> unit
  (** [copy src dst] copies data from [src] to [dst] until end-of-file. *)

  val copy_string : string -> #sink -> unit
  (** [copy_string s = copy (string_source s)] *)

  val buffer_sink : Buffer.t -> sink
  (** [buffer_sink b] is a sink that adds anything sent to it to [b]. *)

  (** {2 Bidirectional streams} *)

  type shutdown_command = [
    | `Receive  (** Indicate that no more reads will be done *)
    | `Send     (** Indicate that no more writes will be done *)
    | `All      (** Indicate that no more reads or writes will be done *)
  ]

  class virtual two_way : object
    inherit source
    inherit sink

    method virtual shutdown : shutdown_command -> unit
  end

  val shutdown : #two_way -> shutdown_command -> unit
  (** [shutdown t cmd] indicates that the caller has finished reading or writing [t]
      (depending on [cmd]).

      This is useful in some protocols to indicate that you have finished sending the request,
      and that the remote peer should now send the response. *)

  (** {2 Closing}

      Flows are usually attached to switches and closed automatically when the switch
      finished. However, it can be useful to close them sooner manually in some cases. *)

  class type close = object
    method close : unit
  end

  val close : #close -> unit
  (** [close t] marks the flow as closed. It can no longer be used after this. *)
end

(** Buffered input and parsing *)
module Buf_read : sig
  (** This module provides fairly efficient non-backtracking parsers.
      It is modelled on Angstrom's API, and you should use that if
      backtracking is needed.

      Example:
      {[
        let r = Buf_read.of_flow flow ~max_size:1_000_000 in
        Buf_read.line r
      ]}
  *)

  type t
  (** An input buffer. *)

  exception Buffer_limit_exceeded
  (** Raised if parsing an item would require enlarging the buffer beyond its configured limit. *)

  type 'a parser = t -> 'a
  (** An ['a parser] is a function that consumes and returns a value of type ['a].
      @raise Failure The flow can't be parsed as a value of type ['a].
      @raise End_of_file The flow ended without enough data to parse an ['a].
      @raise Buffer_limit_exceeded Parsing the value would exceed the configured size limit. *)

  val parse : ?initial_size:int -> max_size:int -> 'a parser -> #Flow.source -> ('a, [> `Msg of string]) result
  (** [parse p flow ~max_size] uses [p] to parse everything in [flow].

      It is a convenience function that does
      {[
        let buf = of_flow flow ~max_size in
        format_errors (p <* eof) buf
      ]}

      @param initial_size see {!of_flow}. *)

  val parse_exn : ?initial_size:int -> max_size:int -> 'a parser -> #Flow.source -> 'a
  (** [parse_exn] wraps {!parse}, but raises [Failure msg] if that returns [Error (`Msg msg)].

      Catching exceptions with [parse] and then raising them might seem pointless,
      but this has the effect of turning e.g. an [End_of_file] exception into a [Failure]
      with a more user-friendly message. *)

  val parse_string : 'a parser -> string -> ('a, [> `Msg of string]) result
  (** [parse_string p s] uses [p] to parse everything in [s].
      It is defined as [format_errors (p <* end_of_input) (of_string s)] *)

  val parse_string_exn : 'a parser -> string -> 'a
  (** [parse_string_exn] is like {!parse_string}, but handles errors like {!parse_exn}. *)

  val of_flow : ?initial_size:int -> max_size:int -> #Flow.source -> t
  (** [of_flow ~max_size flow] is a buffered reader backed by [flow].

      @param initial_size The initial amount of memory to allocate for the buffer.
      @param max_size The maximum size to which the buffer may grow.
                      This must be large enough to hold the largest single item
                      you want to parse (e.g. the longest line, if using
                      {!line}), plus any terminator needed to know the value is
                      complete (e.g. the newline character(s)). This is just to
                      prevent a run-away input from consuming all memory, and
                      you can usually just set it much larger than you expect
                      to need. *)

  val of_buffer : Cstruct.buffer -> t
  (** [of_buffer buf] is a reader that reads from [buf].
      [buf] is used directly, without being copied.
      [eof_seen (of_buffer buf) = true].
      This module will not modify [buf] itself, but it will expose it via {!peek}. *)

  val of_string : string -> t
  (** [of_string s] is a reader that reads from [s]. *)

  val as_flow : t -> Flow.source
  (** [as_flow t] is a buffered flow.

      Reading from it will return data from the buffer,
      only reading the underlying flow if the buffer is empty. *)

  (** {2 Reading data} *)

  val line : string parser
  (** [line] parses one line.

      Lines can be terminated by either LF or CRLF.
      The returned string does not include the terminator.

      If [End_of_file] is reached after seeing some data but before seeing a line
      terminator, the data seen is returned as the last line. *)

  val lines : string Seq.t parser
  (** [lines] returns a sequence that lazily reads the next line until the end of the input is reached.

      [lines = seq line ~stop:at_end_of_input] *)

  val char : char -> unit parser
  (** [char c] checks that the next byte is [c] and consumes it.
      @raise Failure if the next byte is not [c] *)

  val any_char : char parser
  (** [any_char] parses one character. *)

  val peek_char : char option parser
  (** [peek_char] returns [Some c] where [c] is the next character, but does not consume it.

      Returns [None] at the end of the input stream rather than raising [End_of_file]. *)

  val string : string -> unit parser
  (** [string s] checks that [s] is the next string in the stream and consumes it.

      @raise Failure if [s] is not a prefix of the stream. *)

  val take : int -> string parser
  (** [take n] takes exactly [n] bytes from the input. *)

  val take_all : string parser
  (** [take_all] takes all remaining data until end-of-file.

      Returns [""] if already at end-of-file.

      @raise Buffer_limit_exceeded if the remaining data exceeds or equals the buffer limit
             (it needs one extra byte to confirm it has reached end-of-file). *)

  val take_while : (char -> bool) -> string parser
  (** [take_while p] finds the first byte for which [p] is false
      and consumes and returns all bytes before that.

      If [p] is true for all remaining bytes, it returns everything until end-of-file.

      It will return the empty string if there are no matching characters
      (and therefore never raises [End_of_file]). *)

  val skip_while : (char -> bool) -> unit parser
  (** [skip_while p] skips zero or more bytes for which [p] is [true].

      [skip_while p t] does the same thing as [ignore (take_while p t)],
      except that it is not limited by the buffer size. *)

  val skip : int -> unit parser
  (** [skip n] discards the next [n] bytes.

      [skip n] = [map ignore (take n)],
      except that the number of skipped bytes may be larger than the buffer (it will not grow).

      Note: if [End_of_file] is raised, all bytes in the stream will have been consumed. *)

  val at_end_of_input : bool parser
  (** [at_end_of_input] returns [true] when at the end of the stream, or
      [false] if there is at least one more byte to be read. *)

  val end_of_input : unit parser
  (** [end_of_input] checks that there are no further bytes in the stream.
      @raise Failure if there are further bytes *)

  (** {2 Combinators} *)

  val seq : ?stop:bool parser -> 'a parser -> 'a Seq.t parser
  (** [seq p] is a sequence that uses [p] to get the next item.

      A sequence node can only be used while the stream is at
      the expected position, and will raise [Invalid_argument]
      if any bytes have been consumed in the meantime. This
      also means that each node can only be used once; use
      {!Seq.memoize} to make the sequence persistent.

      It is not necessary to consume all the elements of the
      sequence.

      @param stop This is used before parsing each item.
                  The sequence ends if this returns [true].
                  The default is {!at_end_of_input}. *)

  val pair : 'a parser -> 'b parser -> ('a * 'b) parser
  (** [pair a b] is a parser that first uses [a] to parse a value [x],
      then uses [b] to parse a value [y], then returns [(x, y)].

      Note that this module does not support backtracking, so if [b] fails
      then the bytes consumed by [a] are lost. *)

  val return : 'a -> 'a parser
  (** [return x] is a parser that consumes nothing and always returns [x].
      [return] is just [Fun.const]. *)

  val map : ('a -> 'b) -> ('a parser -> 'b parser)
  (** [map f a] is a parser that parses the stream with [a] to get [v],
      and then returns [f v]. *)

  val bind : 'a parser -> ('a -> 'b parser) -> 'b parser
  (** [bind a f] is a parser that first uses [a] to parse a value [v],
      then uses [f v] to select the next parser, and then uses that. *)

  val format_errors : 'a parser -> ('a, [> `Msg of string]) result parser
  (** [format_errors p] catches [Failure], [End_of_file] and
      [Buffer_limit_exceeded] exceptions and returns them as a formatted error message. *)

  (** Convenient syntax for some of the combinators. *)
  module Syntax : sig
    val ( let+ ) : 'a parser -> ('a -> 'b) -> 'b parser
    (** Syntax for {!map}. *)

    val ( let* ) : 'a parser -> ('a -> 'b parser) -> 'b parser
    (** Syntax for {!bind} *)

    val ( and+ ) : 'a parser -> 'b parser -> ('a * 'b) parser
    (** Syntax for {!pair} *)

    val ( and* ) : 'a parser -> 'b parser -> ('a * 'b) parser
    (** Syntax for {!pair} (same as [and+]). *)

    val ( <* ) : 'a parser -> 'b parser -> 'a parser
    (** [a <* b] is [map fst (pair a b)].
        It parses two things and keeps only the first. *)

    val ( *> ) : 'a parser -> 'b parser -> 'b parser
    (** [a *> b] is [map snd (pair a b)].
        It parses two things and keeps only the second. *)
  end

  (** {2 Low-level API} *)

  val buffered_bytes : t -> int
  (** [buffered_bytes t] is the number of bytes that can be read without
      reading from the underlying flow. *)

  val peek : t -> Cstruct.t
  (** [peek t] returns a view onto the active part of [t]'s internal buffer.

      Performing any operation that might add to the buffer may invalidate this,
      so it should be used immediately and then forgotten.

      [Cstruct.length (peek t) = buffered_bytes t]. *)

  val ensure : t -> int -> unit
  (** [ensure t n] ensures that the buffer contains at least [n] bytes of data.

      If not, it reads from the flow until there is.

      [buffered_bytes (ensure t n) >= n].

      @raise End_of_file if the flow ended before [n] bytes were available
      @raise Buffer_limit_exceeded if [n] exceeds the buffer's maximum size *)

  val consume : t -> int -> unit
  (** [consume t n] discards the first [n] bytes from [t]'s buffer.

      Use this after {!peek} to mark some bytes as consumed.

      [buffered_bytes t' = buffered_bytes t - n]

      Note: unlike {!skip}, this will not read data from the underlying flow. *)

  val consumed_bytes : t -> int
  (** [consumed_bytes t] is the total number of bytes consumed.

      i.e. it is the offset into the stream of the next byte to be parsed. *)

  val eof_seen : t -> bool
  (** [eof_seen t] indicates whether we've received [End_of_file] from the underlying flow.

      If so, there will never be any further data beyond what [peek] already returns.

      Note that this returns [false] if we're at the end of the stream but don't know it yet.
      Use {!at_end_of_input} to be sure. *)
end

(** Networking. *)
module Net : sig
  (** Example:
      {[
        let addr = `Tcp (Ipaddr.V4.loopback, 8080)

        let http_get ~net ~stdout addr =
          Switch.run @@ fun sw ->
          let flow = Net.connect ~sw net addr in
          Flow.copy_string "GET / HTTP/1.0\r\n\r\n" flow;
          Flow.shutdown flow `Send;
          Flow.copy flow stdout
        ]}
  *)

  exception Connection_reset of exn

  (** IP addresses. *)
  module Ipaddr : sig
    type 'a t = private string
    (** The raw bytes of the IP address.
        It is either 4 bytes long (for an IPv4 address) or
        16 bytes long (for IPv6). *)

    (** IPv4 addresses. *)
    module V4 : sig
      val any : [> `V4] t
      (** A special IPv4 address, for use only with [listen], representing
          all the Internet addresses that the host machine possesses. *)

      val loopback : [> `V4] t
      (** A special IPv4 address representing the host machine ([127.0.0.1]). *)
    end

    (** IPv6 addresses. *)
    module V6 : sig
      val any : [> `V6] t
      (** A special IPv6 address, for use only with [listen], representing
          all the Internet addresses that the host machine possesses. *)

      val loopback : [> `V6] t
      (** A special IPv6 address representing the host machine ([::1]). *)
    end

    val pp : [< `V4 | `V6] t Fmt.t
    (** [pp] formats IP addresses.
        For IPv6 addresses, it follows {{:http://tools.ietf.org/html/rfc5952}}. *)

    type v4v6 = [`V4 | `V6] t

    val fold :
      v4:([> `V4] t -> 'a) -> 
      v6:([> `V6] t -> 'a) ->
      [< `V4 | `V6] t ->
      'a
    (** [fold ~v4 ~v6 t] is [v4 t] if [t] is an IPv4 address, or [v6 t] if it's an IPv6 address. *)

    val of_raw : string -> v4v6
    (** [of_raw addr] casts [addr] to an IP address.
        @raise Invalid_argument if it is not 4 or 16 bytes long. *)
  end

  (** Network addresses. *)
  module Sockaddr : sig
    type stream = [
      | `Unix of string
      | `Tcp of Ipaddr.v4v6 * int
    ]
    (** Socket addresses that we can build a {! Flow.two_way} for i.e. stream-oriented
        protocols. *)

    type datagram = [
      | `Udp of Ipaddr.v4v6 * int
    ]
    (** Socket addresses that are message-oriented. *)

    type t = [ stream | datagram ]

    val pp : Format.formatter -> [< t] -> unit
  end

  (** {2 Provider Interfaces} *)

  class virtual listening_socket : object
    inherit Generic.t
    method virtual close : unit
    method virtual accept : sw:Switch.t -> <Flow.two_way; Flow.close> * Sockaddr.stream
  end

  class virtual datagram_socket : object
    method virtual send : Sockaddr.datagram -> Cstruct.t -> unit
    method virtual recv : Cstruct.t -> Sockaddr.datagram * int
  end

  class virtual t : object
    method virtual listen : reuse_addr:bool -> reuse_port:bool -> backlog:int -> sw:Switch.t -> Sockaddr.stream -> listening_socket
    method virtual connect : sw:Switch.t -> Sockaddr.stream -> <Flow.two_way; Flow.close>
    method virtual datagram_socket : sw:Switch.t -> Sockaddr.datagram -> datagram_socket
  end

  (** {2 Out-bound Connections} *)

  val connect : sw:Switch.t -> #t -> Sockaddr.stream -> <Flow.two_way; Flow.close>
  (** [connect ~sw t addr] is a new socket connected to remote address [addr].

      The new socket will be closed when [sw] finishes, unless closed manually first. *)

  (** {2 Incoming Connections} *)

  val listen : ?reuse_addr:bool -> ?reuse_port:bool -> backlog:int -> sw:Switch.t -> #t -> Sockaddr.stream -> listening_socket
  (** [listen ~sw ~backlog t addr] is a new listening socket bound to local address [addr].

      The new socket will be closed when [sw] finishes, unless closed manually first.

      For (non-abstract) Unix domain sockets, the path will be removed afterwards.

      @param backlog The number of pending connections that can be queued up (see listen(2)).
      @param reuse_addr Set the {!Unix.SO_REUSEADDR} socket option.
                        For Unix paths, also remove any stale left-over socket.
      @param reuse_port Set the {!Unix.SO_REUSEPORT} socket option. *)

  val accept :
    sw:Switch.t ->
    #listening_socket ->
    <Flow.two_way; Flow.close> * Sockaddr.stream
  (** [accept ~sw socket] waits until a new connection is ready on [socket] and returns it.

      The new socket will be closed automatically when [sw] finishes, if not closed earlier.
      If you want to handle multiple connections, consider using {!accept_sub} instead. *)

  val accept_sub :
    sw:Switch.t ->
    #listening_socket ->
    on_error:(exn -> unit) ->
    (sw:Switch.t -> <Flow.two_way; Flow.close> -> Sockaddr.stream -> unit) ->
    unit
  (** [accept socket fn] accepts a connection and handles it in a new fiber.

      After accepting a connection to [socket], it runs [fn ~sw flow client_addr] in a new fiber,
      using {!Fiber.fork_on_accept}.

      [flow] will be closed automatically when the sub-switch is finished, if not already closed by then. *)

  (** {2 Datagram Sockets} *)

  val datagram_socket : sw:Switch.t -> #t -> Sockaddr.datagram -> datagram_socket
  (** [datagram_socket ~sw t addr] creates a new datagram socket that data can be sent to
      and received from. The new socket will be closed when [sw] finishes. *)

  val send : datagram_socket -> Sockaddr.datagram -> Cstruct.t -> unit
  (** [send sock addr buf] sends the data in [buf] to the address [addr] using the 
      the datagram socket [sock]. *)

  val recv : datagram_socket -> Cstruct.t -> Sockaddr.datagram * int
  (** [recv sock buf] receives data from the socket [sock] putting it in [buf]. The number of bytes received is 
      returned along with the sender address and port. If the [buf] is too small then excess bytes may be discarded
      depending on the type of the socket the message is received from. *)
end

(** Parallel computation across multiple CPU cores. *)
module Domain_manager : sig
  class virtual t : object
    method virtual run_raw : 'a. (unit -> 'a) -> 'a

    method virtual run : 'a. (unit -> 'a) -> 'a
    (** Note: cancellation is handled by the {!run} wrapper function, not the object. *)
  end

  val run : #t -> (unit -> 'a) -> 'a
  (** [run t f] runs [f ()] in a newly-created domain and returns the result.

      Other fibers in the calling domain can run in parallel with the new domain.

      Warning: [f] must only access thread-safe values from the calling domain,
      but this is not enforced by the type system.

      If the calling fiber is cancelled, this is propagated to the spawned domain. *)

  val run_raw : #t -> (unit -> 'a) -> 'a
  (** [run_raw t f] is like {!run}, but does not run an event loop in the new domain,
      and so cannot perform IO, fork fibers, etc. *)
end

(** Clocks, time, sleeping and timeouts. *)
module Time : sig
  class virtual clock : object
    method virtual now : float
    method virtual sleep_until : float -> unit
  end

  val now : #clock -> float
  (** [now t] is the current time according to [t]. *)

  val sleep_until : #clock -> float -> unit
  (** [sleep_until t time] waits until the given time is reached. *)

  val sleep : #clock -> float -> unit
  (** [sleep t d] waits for [d] seconds. *)

  val with_timeout : #clock -> float -> (unit -> ('a, 'e) result) -> ('a, [> `Timeout] as 'e) result
  (** [with_timeout clock d fn] runs [fn ()] but cancels it after [d] seconds. *)

  exception Timeout

  val with_timeout_exn : #clock -> float -> (unit -> 'a) -> 'a
  (** [with_timeout_exn clock d fn] runs [fn ()] but cancels it after [d] seconds,
      raising exception [Timeout]. *)
end

(** Tranditional Unix permissions. *)
module Unix_perm : sig
  type t = int
  (** This is the same as {!Unix.file_perm}, but avoids a dependency on [Unix]. *)
end

(** File-system access. *)
module Dir : sig
  (** A [Dir.t] represents access to a directory and contents, recursively.

      {!Stdenv.fs} provides access to the whole file-system.

      Example:

      {[
        Eio.Dir.load fs "/etc/passwd"
      ]}
  *)

  type path = string

  exception Already_exists of path * exn
  exception Not_found of path * exn
  exception Permission_denied of path * exn

  class virtual rw : object
    inherit Generic.t
    inherit Flow.source
    inherit Flow.sink
  end

  (** When to create a new file. *)
  type create = [
    | `Never                            (** fail if the named file doesn't exist *)
    | `If_missing of Unix_perm.t        (** create if file doesn't already exist *)
    | `Or_truncate of Unix_perm.t       (** any existing file is truncated to zero length *)
    | `Exclusive of Unix_perm.t         (** always create; fail if the file already exists *)
  ]
  (** If a new file is created, the given permissions are used for it. *)

  class virtual t : object
    method virtual open_in : sw:Switch.t -> path -> <Flow.source; Flow.close>
    method virtual open_out :
      sw:Switch.t ->
      append:bool ->
      create:create ->
      path -> <rw; Flow.close>
    method virtual mkdir : perm:Unix_perm.t -> path -> unit
    method virtual open_dir : sw:Switch.t -> path -> t_with_close
    method virtual read_dir : path -> path list
  end
  and virtual t_with_close : object
    inherit t
    method virtual close : unit
  end

  (** {1 Reading files} *)

  val load : #t -> path -> string
  (** [load t path] returns the contents of the given file.

      This is a convenience wrapper around {!with_open_in}. *)

  val open_in : sw:Switch.t -> #t -> path -> <Flow.source; Flow.close>
  (** [open_in ~sw t path] opens [t/path] for reading.

      Note: files are always opened in binary mode. *)

  val with_open_in : #t -> path -> (<Flow.source; Flow.close> -> 'a) -> 'a
  (** [with_open_in] is like [open_in], but calls [fn flow] with the new flow and closes
      it automatically when [fn] returns (if it hasn't already been closed by then). *)

  val with_lines : #t -> path -> (string Seq.t -> 'a) -> 'a
  (** [with_lines t path fn] is a convenience function for streaming the lines of the file.

      It uses {!Buf_read.lines}. *)

  (** {1 Writing files} *)

  val save : ?append:bool -> create:create -> #t -> path -> string -> unit
  (** [save t path data ~create] writes [data] to [path].

      This is a convenience wrapper around {!with_open_out}. *)

  val open_out :
    sw:Switch.t ->
    ?append:bool ->
    create:create ->
    #t -> path -> <rw; Flow.close>
  (** [open_out ~sw t path] opens [t/path] for reading and writing.

      Note: files are always opened in binary mode.
      @param append Open for appending: always write at end of file.
      @param create Controls whether to create the file, and what permissions to give it if so. *)

  val with_open_out :
    ?append:bool ->
    create:create ->
    #t -> path -> (<rw; Flow.close> -> 'a) -> 'a
  (** [with_open_out] is like [open_out], but calls [fn flow] with the new flow and closes
      it automatically when [fn] returns (if it hasn't already been closed by then). *)

  (** {1 Directories} *)

  val mkdir : #t -> perm:Unix.file_perm -> path -> unit
  (** [mkdir t ~perm path] creates a new directory [t/path] with permissions [perm]. *)

  val open_dir : sw:Switch.t -> #t -> path -> <t; Flow.close>
  (** [open_dir ~sw t path] opens [t/path].

      This can be passed to functions to grant access only to the subtree [t/path]. *)

  val with_open_dir : #t -> path -> (<t; Flow.close> -> 'a) -> 'a
  (** [with_open_dir] is like [open_dir], but calls [fn dir] with the new directory and closes
      it automatically when [fn] returns (if it hasn't already been closed by then). *)

  val read_dir : #t -> path -> string list
  (** [read_dir t path] reads directory entries for [t/path]. The entries are sorted using {! String.compare}.*)
end

(** The standard environment of a process. *)
module Stdenv : sig
  (** All access to the outside world comes from running the event loop,
      which provides a {!t}.

      Example:
      {[
        let () =
          Eio_main.run @@ fun env ->
          Eio.Dir.with_open_dir env#fs "/srv/www" @@ fun www ->
          serve_files www
            ~net:env#net
      ]}
  *)

  type t = <
    stdin  : Flow.source;
    stdout : Flow.sink;
    stderr : Flow.sink;
    net : Net.t;
    domain_mgr : Domain_manager.t;
    clock : Time.clock;
    fs : Dir.t;
    cwd : Dir.t;
    secure_random : Flow.source;
  >

  (** {1 Standard streams}

      To use these, see {!Flow}. *)

  val stdin  : <stdin  : #Flow.source as 'a; ..> -> 'a
  val stdout : <stdout : #Flow.sink   as 'a; ..> -> 'a
  val stderr : <stderr : #Flow.sink   as 'a; ..> -> 'a

  (** {1 File-system access}

      To use these, see {!Dir}. *)

  val cwd : <cwd : #Dir.t as 'a; ..> -> 'a
  (** [cwd t] is the current working directory of the process (this may change
      over time if the process does a "chdir" operation, which is not recommended). *)

  val fs : <fs : #Dir.t as 'a; ..> -> 'a
  (** [fs t] is the process's full access to the filesystem.

      Paths can be absolute or relative (to the current working directory).
      Using relative paths with this is similar to using them with {!cwd},
      except that this will follow ".." and symlinks to other parts of the filesystem.

      [fs] is useful for handling paths passed in by the user. *)

  (** {1 Network}

      To use this, see {!Net}.
  *)

  val net : <net : #Net.t as 'a; ..> -> 'a
  (** [net t] gives access to the process's network namespace. *)

  (** {1 Domains (using multiple CPU cores)}

      To use this, see {!Domain_manager}.
  *)

  val domain_mgr : <domain_mgr : #Domain_manager.t as 'a; ..> -> 'a
  (** [domain_mgr t] allows running code on other cores. *)

  (** {1 Time}

      To use this, see {!Time}.
  *)

  val clock : <clock : #Time.clock as 'a; ..> -> 'a
  (** [clock t] is the system clock. *)

  (** {1 Randomness} *)

  val secure_random : <secure_random : #Flow.source as 'a; ..> -> 'a
  (** [secure_random t] is a source of random bytes suitable for cryptographic purposes. *)

end

(** {1 Errors and Debugging} *)

val traceln :
  ?__POS__:string * int * int * int ->
  ('a, Format.formatter, unit, unit) format4 -> 'a
(** [traceln fmt] outputs a debug message (typically to stderr).

    Trace messages are printed by default and do not require logging to be configured first.
    The message is printed with a newline, and is flushed automatically.
    [traceln] is intended for quick debugging rather than for production code.

    Unlike most Eio operations, [traceln] will never switch to another fiber;
    if the OS is not ready to accept the message then the whole domain waits.

    It is safe to call [traceln] from multiple domains at the same time.
    Each line will be written atomically.

    Examples:
    {[
      traceln "x = %d" x;
      traceln "x = %d" x ~__POS__;   (* With location information *)
    ]}
    @param __POS__ Display [__POS__] as the location of the [traceln] call. *)


(** Reporting multiple failures at once. *)
module Exn : sig
  type with_bt = exn * Printexc.raw_backtrace

  exception Multiple of exn list
  (** Raised if multiple fibers fail, to report all the exceptions. *)

  val combine : with_bt -> with_bt -> with_bt
  (** [combine x y] returns a single exception and backtrace to use to represent two errors.

      Only one of the backtraces will be kept.
      The resulting exception is typically just [Multiple [y; x]],
      but various heuristics are used to simplify the result:
      - Combining with a {!Cancel.Cancelled} exception does nothing, as these don't need to be reported.
        The result is only [Cancelled] if there is no other exception available.
      - If [x] is a [Multiple] exception then [y] is added to it, to avoid nested [Multiple] exceptions.
      - Duplicate exceptions are removed (using physical equality of the exception). *)
end

(** {1 Provider API for OS schedulers} *)

(** API for use by the scheduler implementation. *)
module Private : sig
  (** Every fiber has an associated context. *)
  module Fiber_context : sig
    type t

    val make_root : unit -> t
    (** Make a new root context for a new domain. *)

    val make : cc:Cancel.t -> t
    (** [make ~cc] is a new fiber context, initially attached to the given cancellation context. *)

    val destroy : t -> unit
    (** [destroy t] removes [t] from its cancellation context. *)

    val tid : t -> Ctf.id

    (** {2 Cancellation}

        The {!Cancel} module describes the user's view of cancellation.

        Internally, when the user calls a primitive operation that needs to block the fiber,
        the [Suspend callback] effect is performed.
        This suspends the fiber and calls [callback] from the scheduler's context,
        passing it the suspended fiber's context.
        If the operation can be cancelled,
        the callback should use {!set_cancel_fn} to register a cancellation function.

        There are two possible outcomes for the operation: it may complete normally,
        or it may be cancelled.
        If it is cancelled then the registered cancellation function is called.
        This function will always be called from the fiber's own domain, but care must be taken
        if the operation is being completed by another domain at the same time.

        Consider the case of {!Stream.take}, which can be fulfilled by a {!Stream.add} from another domain.
        We want to ensure that either the item is removed from the stream and returned to the waiting fiber,
        or that the operation is cancelled and the item is not removed from the stream.

        Therefore, cancelling and completing both attempt to clear the cancel function atomically,
        so that only one can succeed. The case where [Stream.take] succeeds before cancellation:

        + A fiber calls [Suspend] and is suspended.
          The callback sets a cancel function and registers a waiter on the stream.
        + When another domain has an item, it removes the cancel function (making the [take] uncancellable)
          and begins resuming the fiber with the new item.
        + If the taking fiber is cancelled after this, the cancellation will be ignored and the operation
          will complete successfully. Future operations will fail immediately, however.

        The case of cancellation winning the race:

        + A fiber calls [Suspend] and is suspended.
          The callback sets a cancel function and registers a waiter on the stream.
        + The taking fiber is cancelled. Its cancellation function is called, which starts removing the waiter.
        + If another domain tries to provide an item to the waiter as this is happening,
          it will try to clear the cancel function and fail.
          The item will be given to the next waiter instead.

        Note that there is a mutex around the list of waiters, so the taking domain
        can't finish removing the waiter and start another operation while the adding
        domain is trying to resume it.
        In future, we may want to make this lock-free by using a fresh atomic
        to hold the cancel function for each operation.

        Note: A fiber will only have a cancel function set while it is suspended. *)

    val cancellation_context : t -> Cancel.t
    (** [cancellation_context t] is [t]'s current cancellation context. *)

    val set_cancel_fn : t -> (exn -> unit) -> unit
    (** [set_cancel_fn t fn] sets [fn] as the fiber's cancel function.

        If the cancellation context is cancelled, the function is removed and called.
        When the operation completes, you must call {!clear_cancel_fn} to remove it. *)

    val clear_cancel_fn : t -> bool
    (** [clear_cancel_fn t] removes the function previously set with {!set_cancel_fn}, if any.

        Returns [true] if this call removed the function, or [false] if there wasn't one.
        This operation is atomic and thread-safe.
        An operation that completes in another domain must use this to indicate that the operation is
        finished (can no longer be cancelled) before enqueuing the result. If it returns [false],
        the operation was cancelled first and the canceller has called (or is calling) the function.
        If it returns [true], the caller is responsible for any resources owned by the function,
        such as the continuation. *)

    val get_error : t -> exn option
    (** [get_error t] is [Cancel.get_error (cancellation_context t)] *)
  end

  (** Temporary hack for compatibility with ocaml.4.12+domains *)
  module Effect = Effect

  module Effects : sig
    type 'a enqueue = ('a, exn) result -> unit
    (** A function provided by the scheduler to reschedule a previously-suspended thread. *)

    type _ Effect.t +=
      | Suspend : (Fiber_context.t -> 'a enqueue -> unit) -> 'a Effect.t
      (** [Suspend fn] is performed when a fiber must be suspended
          (e.g. because it called {!Promise.await} on an unresolved promise).
          The effect handler runs [fn fiber enqueue] in the scheduler context,
          passing it the suspended fiber's context and a function to resume it.
          [fn] should arrange for [enqueue] to be called once the thread is ready to run again. *)

      | Fork : Fiber_context.t * (unit -> unit) -> unit Effect.t
      (** [perform (Fork new_context f)] creates a new fiber and runs [f] in it, with context [new_context].
          [f] must not raise an exception. See {!Fiber.fork}. *)

      | Trace : (?__POS__:(string * int * int * int) -> ('a, Format.formatter, unit, unit) format4 -> 'a) Effect.t
      (** [perform Trace fmt] writes trace logging to the configured trace output.
          It must not switch fibers, as tracing must not affect scheduling.
          If the system is not ready to receive the trace output,
          the whole domain must block until it is. *)

      | Get_context : Fiber_context.t Effect.t
      (** [perform Get_context] immediately returns the current fiber's context (without switching fibers). *)
  end

  module Ctf = Ctf
end
