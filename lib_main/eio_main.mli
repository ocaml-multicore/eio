(** Select a suitable event loop for Eio. *)

val run : (Eio.Stdenv.t -> 'a) -> 'a
(** [run fn] runs an event loop and then calls [fn env] within it.

    [env] provides access to the process's environment (file-system, network, etc).

    When [fn] ends, the event loop finishes.

    This should be called once, at the entry point of an application.
    It {b must not} be called by libraries.
    Doing so would force the library to depend on Unix
    (making it unusable from unikernels or browsers),
    prevent the user from choosing their own event loop,
    and prevent using the library with other Eio libraries.

    [run] will select an appropriate event loop for the current platform.
    On many systems, it will use {!Eio_luv.run}.

    On recent-enough versions of Linux, it will use {!Eio_linux.run}.
    You can override this by setting the $EIO_BACKEND environment variable to
    either "io-uring" or "luv". *)
