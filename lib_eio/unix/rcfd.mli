(** A safe wrapper around [Unix.file_descr].

    When FDs are shared between domains, there is the risk that one domain will try to use an FD
    just as another domain is closing it. Then this can happen:

    + Domain A decides to write to FD 3, which it shares with domain B.
    + Domain B closes FD 3.
    + Domain C opens a new file, getting assigned FD 3 by the OS.
    + Domain A writes to FD 3, corrupting domain C's file.

    This would break modularity, since the fibers in domains A and C may have no connection with each other.

    To prevent this, we keep a ref-count, tracking how many fibers are using the FD.
    Wrap uses of the FD in {!use} to ensure that it won't be closed while you're using it.

    Calling {!close} while one or more operations are still in progress marks
    the wrapper as closing (so that no futher operations can start); the last
    operation to finish will close the underlying FD. *)

type t

val make : Unix.file_descr -> t
(** [let t = make fd] wraps [fd].

    [t] takes ownership of [fd]. The caller is responsible for ensuring that {!close}
    (or {!remove}) is called at least once in the future. *)

val use : if_closed:(unit -> 'a) -> t -> (Unix.file_descr -> 'a) -> 'a
(** [use t fn ~if_closed] calls [fn fd], preventing [fd] from being closed until [fn] returns.

    [if_closed ()] is used if [t] is closed before we can increment the ref-count.
    [use] can be used in parallel from multiple domains at the same time.

    This operation is lock-free and can be used safely from signal handlers, etc. *)

val is_open : t -> bool
(** [is_open t] returns [true] until [t] has been marked as closing, after which it returns [false].

    This is mostly useful inside the callback of {!use}, to test whether
    another fiber has started closing [t] (in which case you may decide to stop early). *)

val close : t -> bool
(** [close t] marks [t] as closed and arranges for its FD to be closed.

    If there are calls to {!use} in progress, the last one to finish will close the underlying FD.
    Note that this function returns without waiting for the close to happen in that case.

    Returns [true] after successfully marking [t] as closing, or [false] if it was already marked.

    If you need to wait until the underlying FD is closed, use {!remove} and then close the FD yourself instead. *)

val remove : t -> Unix.file_descr option
(** [remove t] closes [t] and returns the FD.

    It immediately marks [t] as closing (so no further operations can start)
    and then waits until there are no further users.

    This operation suspends the calling fiber and so must run from an Eio fiber.
    It does not allow itself to be cancelled,
    since it takes ownership of the FD and that would be leaked if it aborted.

    If another fiber marks [t] as closing before [remove] can, it returns [None] immediately. *)

val peek : t -> Unix.file_descr
(** [peek t] returns the file-descriptor without updating the ref-count.

    You must ensure that [t] isn't closed while using the result.
    This is a dangerous operation and may be removed in the future.

    If [t] was closed, it instead raises an exception (if you're not sure when
    [t] might get closed, you shouldn't be using this function). *)

val pp : t Fmt.t
(** Displays the FD number. *)
