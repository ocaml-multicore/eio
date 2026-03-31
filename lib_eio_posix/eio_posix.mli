(** Fallback Eio backend for POSIX systems. *)

type stdenv = Eio_unix.Stdenv.base
(** The type of the standard set of resources available on POSIX systems. *)

val run : ?pipe:(cloexec:bool -> Unix.file_descr * Unix.file_descr) -> (stdenv -> 'a) -> 'a
(** [run main] runs an event loop and calls [main stdenv] inside it.

    For portable code, you should use {!Eio_main.run} instead, which will call this for you if appropriate.

    @param pipe Configure how to create a pipe for the underlying scheduler (defaults to {! Unix.pipe}). *)

module Low_level = Low_level
(** Low-level API for making POSIX calls directly. *)
