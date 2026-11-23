(** Eio backend using Windows completion ports for streams, datagrams, pipes and
    positioned I/O. Synchronous handles use worker threads; explicit readiness
    waits and Unix-domain connects retain the readiness fallback.

    Cancelling a synchronous pipe write can leave a prefix at the reader.
    Windows does not report the length of that prefix.

    On Unix-domain streams, Winsock may accept writes after the peer closes.
    A subsequent write can report {!Eio.Net.Connection_reset}. *)

type stdenv = Eio_unix.Stdenv.base
(** An extended version of {!Eio.Stdenv.base} with some extra features available on Windows. *)

val run : (stdenv -> 'a) -> 'a
(** [run main] runs an event loop and calls [main stdenv] inside it.

    For portable code, you should use {!Eio_main.run} instead, which will call this for you if appropriate. *)

module Low_level = Low_level
(** Low-level API. *)
