(** Private internal module. Use {!Eio} instead. *)

(** @canonical Eio.Switch *)
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
      Once all fibers within the switch have finished, these hooks are called.
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

(** @canonical Eio.Promise *)
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

  type +!'a t
  (** An ['a t] is a promise for a value of type ['a]. *)

  type -!'a u
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

(** @canonical Eio.Fiber *)
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

      If both fibers fail, {!Exn.combine} is used to combine the exceptions.

      Warning: it is always possible that {i both} operations will succeed (and one result will be thrown away).
      This is because there is a period of time after the first operation succeeds,
      but before its fiber finishes, during which the other operation may also succeed. *)

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

  val fork_promise : sw:Switch.t -> (unit -> 'a) -> 'a Promise.or_exn
  (** [fork_promise ~sw fn] schedules [fn ()] to run in a new fiber and returns a promise for its result.

      This is just a convenience wrapper around {!fork}.
      If [fn] raises an exception then the promise is resolved to the error, but [sw] is not failed. *)

  val fork_daemon : sw:Switch.t -> (unit -> [`Stop_daemon]) -> unit
  (** [fork_daemon] is like {!fork} except that instead of waiting for the fiber to finish,
      the switch will cancel it once all non-daemon fibers are done.

      The switch will still wait for the daemon fiber to finish cancelling.

      The return type of [[`Stop_daemon]] instead of [unit] is just to catch mistakes,
      as daemons normally aren't expected to return. *)

  val check : unit -> unit
  (** [check ()] checks that the fiber's context hasn't been cancelled.
      Many operations automatically check this before starting.
      @raise Cancel.Cancelled if the fiber's context has been cancelled. *)

  val yield : unit -> unit
  (** [yield ()] asks the scheduler to switch to the next runnable task.
      The current task remains runnable, but goes to the back of the queue.
      Automatically calls {!check} just before resuming. *)

  (** Concurrent list operations. *)
  module List : sig
    (** These functions behave like the ones in the standard library's [List]
        module, except that multiple items can be processed concurrently.

        They correspond to Lwt's [Lwt_list.*_p] operations. e.g.
        [Lwt_list.iter_p] becomes [Fiber.List.iter].
        For the [Lwt_list.*_s] operations, just use the standard library function.
        e.g. [Lwt_list.iter_s] can be replaced by a plain [List.iter]. *)

    val filter : ?max_fibers:int -> ('a -> bool) -> 'a list -> 'a list
    (** [filter f x] is like [List.filter f x] except that the invocations of [f] are
        run concurrently in separate fibers.
        @param max_fibers Maximum number of fibers to run concurrently *)

    val map : ?max_fibers:int -> ('a -> 'b) -> 'a list -> 'b list
    (** [map f x] is like [List.map f x] except that the invocations of [f] are
        run concurrently in separate fibers.
        @param max_fibers Maximum number of fibers to run concurrently *)

    val filter_map : ?max_fibers:int -> ('a -> 'b option) -> 'a list -> 'b list
    (** [filter_map f x] is like [List.filter_map f x] except that the
        invocations of [f] are run concurrently in separate fibers.
        @param max_fibers Maximum number of fibers to run concurrently *)

    val iter : ?max_fibers:int -> ('a -> unit) -> 'a list -> unit
    (** [iter f x] is like [List.iter f x] except that the invocations of [f] are
        run concurrently in separate fibers.
        @param max_fibers Maximum number of fibers to run concurrently *)
  end

  val filter : ?max_fibers:int -> ('a -> bool) -> 'a list -> 'a list
  [@@ocaml.deprecated "Use `Eio.Fiber.List.filter` instead."]

  val map : ?max_fibers:int -> ('a -> 'b) -> 'a list -> 'b list
  [@@ocaml.deprecated "Use `Eio.Fiber.List.map instead."]

  val filter_map : ?max_fibers:int -> ('a -> 'b option) -> 'a list -> 'b list
  [@@ocaml.deprecated "Use `Eio.Fiber.List.filter_map instead."]

  val iter : ?max_fibers:int -> ('a -> unit) -> 'a list -> unit
  [@@ocaml.deprecated "Use `Eio.Fiber.List.iter instead."]

  (** {2 Fiber-local variables}

      Each fiber maintains a map of additional variables associated with it,
      which can be used to store fiber-related state or context. This map is
      propagated to any forked fibers.

      While fiber-local variables can be useful, they can also make code much
      harder to reason about, as they effectively act as another form of global
      state. When possible, prefer passing arguments around explicitly.

      Fiber-local variables are particularly useful for attaching extra
      information for debugging, such as a request ID that the log system can
      include in all logged messages.
      *)

  type 'a key
  (** ['a key] is a fiber-local variable of type ['a].

      Since the key is required to get or set a variable, a library can keep its
      key private to control how the variable can be accessed. *)

  val create_key : unit -> 'a key
  (** [create_key ()] creates a new fiber-local variable. *)

  val get : 'a key -> 'a option
  (** [get key] reads [key] from the map of fiber local variables, returning its
      value or {!None} if it has not been bound. *)

  val with_binding : 'a key -> 'a -> (unit -> 'b) -> 'b
  (** [with_binding key value fn] runs [fn] with [key] bound to the provided
      [value].

      Whilst this binding only exists for the duration of this function {i on
      this fiber}, it will be propagated to any forked fibers. If [fn] creates
      fibers using an external switch, the bound value may be continue to be
      used after this function returns. *)

  val without_binding : 'a key -> (unit -> 'b) -> 'b
  (** [with_binding key value fn] runs [fn] with any binding for [key] removed.
      *)
end

(** @canonical Eio.Exn *)
module Exn : sig
  type with_bt = exn * Printexc.raw_backtrace

  type err = ..
  (** Describes the particular error that occurred.

      They are typically nested (e.g. [Fs (Permission_denied (Unix_error ...))])
      so that you can match e.g. all IO errors, all file-system errors, all
      permission denied errors, etc.

      If you extend this, use {!register_pp} to add a printer for the new error. *)

  type context
  (** Extra information attached to an IO error.
      This provides contextual information about what caused the error. *)

  exception Io of err * context
  (** A general purpose IO exception.

      This is used for most errors interacting with the outside world,
      and is similar to {!Unix.Unix_error}, but more general.
      An unknown [Io] error should typically be reported to the user, but does
      not generally indicate a bug in the program. *)

  type err += Multiple_io of (err * context * Printexc.raw_backtrace) list
  (** Error code used when multiple IO errors occur.

      This is useful if you want to catch and report all IO errors. *)

  val create : err -> exn
  (** [create err] is an {!Io} exception with an empty context. *)

  val add_context : exn -> ('a, Format.formatter, unit, exn) format4 -> 'a
  (** [add_context ex msg] returns a new exception with [msg] added to [ex]'s context,
      if [ex] is an {!Io} exception.

      If [ex] is not an [Io] exception, this function just returns the original exception. *)

  val reraise_with_context : exn -> Printexc.raw_backtrace -> ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** [reraise_with_context ex bt msg] raises [ex] extended with additional information [msg].

      [ex] should be an {!Io} exception (if not, is re-raised unmodified).

      Example:
      {[
         try connect addr
         with Eio.Io _ as ex ->
           let bt = Printexc.get_raw_backtrace () in
           reraise_with_context ex bt "connecting to %S" addr
      ]}

      You must get the backtrace before calling any other function
      in the exception handler to prevent corruption of the backtrace. *)

  val register_pp : (Format.formatter -> err -> bool) -> unit
  (** [register_pp pp] adds [pp] as a pretty-printer of errors.

      [pp f err] should format [err] using [f], if possible.
      It should return [true] on success, or [false] if it didn't
      recognise [err]. *)

  val pp : exn Fmt.t
  (** [pp] is a formatter for exceptions.

      This is similar to {!Fmt.exn}, but can do a better job on {!Io} exceptions
      because it can format them directly without having to convert to a string first. *)

  (** Extensible backend-specific exceptions. *)
  module Backend : sig
    type t = ..

    val show : bool ref
    (** Controls the behaviour of {!pp}. *)

    val register_pp : (Format.formatter -> t -> bool) -> unit
    (** [register_pp pp] adds [pp] as a pretty-printer of backend errors.

        [pp f err] should format [err] using [f], if possible.
        It should return [true] on success, or [false] if it didn't
        recognise [err]. *)

    val pp : t Fmt.t
    (** [pp] behaves like {!pp} except that if display of backend errors has been turned off
        (with {!show}) then it just prints a place-holder.

        This is useful for formatting the backend-specific part of exceptions,
        which should be hidden in expect-style testing that needs to work on multiple backends. *)
  end

  type err += X of Backend.t
  (** A top-level code for backend errors that don't yet have a cross-platform classification in Eio.

      You should avoid matching on these (in portable code). Instead, request a proper Eio code for them. *)

  exception Multiple of with_bt list
  (** Raised if multiple fibers fail, to report all the exceptions.

      This usually indicates a bug in the program.

      Note: If multiple {b IO} errors occur, then you will get [Io (Multiple_io _, _)] instead of this. *)

  val combine : with_bt -> with_bt -> with_bt
  (** [combine x y] returns a single exception and backtrace to use to represent two errors.

      The resulting exception is typically just [Multiple [y; x]],
      but various heuristics are used to simplify the result:
      - Combining with a {!Cancel.Cancelled} exception does nothing, as these don't need to be reported.
        The result is only [Cancelled] if there is no other exception available.
      - If both errors are [Io] errors, then the result is [Io (Multiple_io _)]. *)
end

(** @canonical Eio.Cancel *)
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

      When handling a [Cancelled] exception, quick clean-up actions
      (such as releasing a mutex or deleting a temporary file) are OK,
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

(** @canonical Eio.Private *)
module Private : sig
  module Ctf = Ctf

  (** Every fiber has an associated context. *)
  module Fiber_context : sig
    type t

    val make_root : unit -> t
    (** Make a new root context for a new domain. *)

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
        if the operation could be completed by another domain at the same time.

        Consider the case of {!Stream.take}, which can be fulfilled by a {!Stream.add} from another domain.
        We want to ensure that either the item is removed from the stream and returned to the waiting fiber,
        or that the operation is cancelled and the item is not removed from the stream.

        Therefore, cancelling and completing both need to update an atomic value (with {!Atomic.compare_and_set})
        so that only one can succeed. The case where [Stream.take] succeeds before cancellation:

        + A fiber calls [Suspend] and is suspended.
          The callback sets a cancel function and registers a waiter on the stream.
        + When another domain has an item, it marks the atomic as finished (making the [take] uncancellable)
          and begins resuming the fiber with the new item.
        + If the taking fiber is cancelled after this, the cancellation must be ignored and the operation
          will complete successfully. Future operations will fail immediately, however.

        The case of cancellation winning the race:

        + A fiber calls [Suspend] and is suspended.
          The callback sets a cancel function and registers a waiter on the stream.
        + The taking fiber is cancelled. Its cancellation function is called,
          which updates the atomic and starts removing the waiter.
        + If another domain tries to provide an item to the waiter as this is happening,
          it will try to update the atomic too and fail.
          The item will be given to the next waiter instead.

        Note: A fiber will only have a cancel function set while it is suspended. *)

    val cancellation_context : t -> Cancel.t
    (** [cancellation_context t] is [t]'s current cancellation context. *)

    val set_cancel_fn : t -> (exn -> unit) -> unit
    (** [set_cancel_fn t fn] sets [fn] as the fiber's cancel function.

        If [t]'s cancellation context is cancelled, the function is called.
        It should attempt to make the current operation finish quickly, either with
        a successful result or by raising the given exception.

        Just before being called, the fiber's cancel function is replaced with [ignore]
        so that [fn] cannot be called twice.

        On success, the cancel function is cleared automatically when {!Suspend.enter} returns,
        but for single-domain operations you may like to call {!clear_cancel_fn}
        manually to remove it earlier.

        [fn] will be called from [t]'s domain (from the fiber that called [cancel]).

        [fn] must not switch fibers. If it did, this could happen:

        + Another suspended fiber in the same cancellation context resumes before
          its cancel function is called.
        + It enters a protected block and starts a new operation.
        + [fn] returns.
        + We cancel the protected operation. *)

    val clear_cancel_fn : t -> unit
    (** [clear_cancel_fn t] is [set_cancel_fn t ignore].

        This must only be called from the fiber's own domain.

        For single-domain operations, it can be useful to call this manually as soon as
        the operation succeeds (i.e. when the fiber is added to the run-queue)
        to prevent the cancel function from being called.

        For operations where another domain may resume the fiber, your cancel function
        will need to cope with being called after the operation has succeeded. In that
        case you should not call [clear_cancel_fn]. The backend will do it automatically
        just before resuming your fiber. *)

    val get_error : t -> exn option
    (** [get_error t] is [Cancel.get_error (cancellation_context t)] *)
  end

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

      | Get_context : Fiber_context.t Effect.t
      (** [perform Get_context] immediately returns the current fiber's context (without switching fibers). *)
  end

  (** Suspend a fiber and enter the scheduler. *)
  module Suspend : sig
    val enter : (Fiber_context.t -> 'a Effects.enqueue -> unit) -> 'a
    (** [enter fn] suspends the calling fiber and calls [fn ctx enqueue] in the scheduler's context.
        This should arrange for [enqueue] to be called when the fiber should be resumed.
        [enqueue] is thread-safe and so can be called from another domain or systhread.

        [ctx] should be used to set a cancellation function. Otherwise, the operation is non-interruptable.
        If the caller's cancellation context is already cancelled, [enter] immediately aborts. *)

    val enter_unchecked : (Fiber_context.t -> 'a Effects.enqueue -> unit) -> 'a
    (** [enter_unchecked] is like [enter] except that it does not perform the initial check
        that the fiber isn't cancelled (this is useful if you want to do the check yourself, e.g.
        because you need to unlock a mutex if cancelled). *)
  end

  (** A queue of fibers waiting for an event. *)
  module Waiters : sig
    type 'a t
    (* A queue of fibers waiting for something.
       Note: an [_ t] is not thread-safe itself.
       To use share it between domains, the user is responsible for wrapping it in a mutex. *)

    val create : unit -> 'a t

    val wake_all : 'a t -> 'a -> unit
    (** [wake_all t] calls (and removes) all the functions waiting on [t].
        If [t] is shared between domains, the caller must hold the mutex while calling this. *)

    val wake_one : 'a t -> 'a -> [`Ok | `Queue_empty]
    (** [wake_one t] is like {!wake_all}, but only calls (and removes) the first waiter in the queue.
        If [t] is shared between domains, the caller must hold the mutex while calling this. *)

    val is_empty : 'a t -> bool
    (** [is_empty t] checks whether there are any functions waiting on [t].
        If [t] is shared between domains, the caller must hold the mutex while calling this,
        and the result is valid until the mutex is released. *)

    val await :
      mutex:Mutex.t option ->
      'a t -> Ctf.id -> 'a
    (** [await ~mutex t id] suspends the current fiber and adds its continuation to [t].
        When the waiter is woken, the fiber is resumed and returns the result.
        If [t] can be used from multiple domains:
        - [mutex] must be set to the mutex to use to unlock it.
        - [mutex] must be already held when calling this function, which will unlock it before blocking.
        When [await] returns, [mutex] will have been unlocked.
        @raise Cancel.Cancelled if the fiber's context is cancelled *)

    val await_internal :
      mutex:Mutex.t option ->
      'a t -> Ctf.id -> Fiber_context.t ->
      (('a, exn) result -> unit) -> unit
    (** [await_internal ~mutex t id ctx enqueue] is like [await], but the caller has to suspend the fiber.
        This also allows wrapping the [enqueue] function.
        Calls [enqueue (Error (Cancelled _))] if cancelled.
        Note: [enqueue] is called from the triggering domain,
              which is currently calling {!wake_one} or {!wake_all}
              and must therefore be holding [mutex]. *)
  end

  module Debug : sig
    val traceln :
      ?__POS__:string * int * int * int ->
      ('a, Format.formatter, unit, unit) format4 -> 'a
    (** Writes trace logging using the current fiber's configured traceln function. *)

    val traceln_mutex : Stdlib.Mutex.t
    (** The mutex used to prevent two domains writing to stderr at once.

        This might be useful if you want to write to it directly yourself,
        e.g. for a log reporter. *)

    val default_traceln :
      ?__POS__:string * int * int * int ->
      ('a, Format.formatter, unit, unit) format4 -> 'a
      (** [default_traceln] is a suitable default implementation for {!Eio.Std.traceln}.

          It writes output to stderr, prefixing each line with a "+".
          If [__POS__] is given, it also displays the file and line number from that.
          It uses {!traceln_mutex} so that only one domain's output is written at a time. *)

    type traceln = {
      traceln : 'a. ?__POS__:string * int * int * int -> ('a, Format.formatter, unit, unit) format4 -> 'a;
    } [@@unboxed]

    type t = <
      traceln : traceln Fiber.key;
    >

    val v : t
    (** Backends should use this for {!Eio.Stdenv.debug}. *)
  end

end
