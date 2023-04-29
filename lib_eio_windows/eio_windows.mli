(** Fallback Eio backend for Windows using OCaml's [Unix.select]. *)

type stdenv = <
  stdin  : <Eio.Flow.source; Eio_unix.Resource.t>;
  stdout : <Eio.Flow.sink; Eio_unix.Resource.t>;
  stderr : <Eio.Flow.sink; Eio_unix.Resource.t>;
  net : Eio.Net.t;
  domain_mgr : Eio.Domain_manager.t;
  clock : Eio.Time.clock;
  mono_clock : Eio.Time.Mono.t;
  fs : Eio.Fs.dir Eio.Path.t;
  cwd : Eio.Fs.dir Eio.Path.t;
  secure_random : Eio.Flow.source;
  debug : Eio.Debug.t;
>
(** An extended version of {!Eio.Stdenv.t} with some extra features available on Windows. *)

val run : (stdenv -> 'a) -> 'a
(** [run main] runs an event loop and calls [main stdenv] inside it.

    For portable code, you should use {!Eio_main.run} instead, which will call this for you if appropriate. *)

module Low_level = Low_level
(** Low-level API. *)
