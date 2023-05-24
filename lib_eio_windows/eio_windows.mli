(** Fallback Eio backend for Windows using OCaml's [Unix.select]. *)

type stdenv = Eio_unix.Stdenv.base
(** An extended version of {!Eio.Stdenv.base} with some extra features available on Windows. *)

val run : (stdenv -> 'a) -> 'a
(** [run main] runs an event loop and calls [main stdenv] inside it.

    For portable code, you should use {!Eio_main.run} instead, which will call this for you if appropriate. *)

module Low_level = Low_level
(** Low-level API. *)
