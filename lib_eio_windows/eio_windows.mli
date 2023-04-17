(** Eio backend for Windows using IOCP *)

val run : (Eio.Stdenv.t -> 'a) -> 'a
(** [run main] runs an event loop and calls [main stdenv] inside it.

    For portable code, you should use {!Eio_main.run} instead, which will call this for you if appropriate. *)