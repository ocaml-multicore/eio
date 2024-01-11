(** Eio backend for JavaScript environments.

    You will normally not use this module directly.
    Instead, use {!Eio_brr.start} or {!Js_of_ocaml_eio.Eio_js.start} and then use the API in the {!Eio} module.
*)

val start : (unit -> unit) -> unit
(** [start fn] executes function [fn] in an environment where Eio
    effects can be performed. The function is executed asynchronously:
    [start fn] returns when all fibers are suspended, without waiting
    for them to complete.

    If [fn ()] raises an exception, the exception is passed to a
    handler that by default outputs the exception to [stderr] and
    exits the program (when this makes sense, that is, in particular,
    not in a browser environment).

    You should not expect effect handlers set-up outside of [start fn]
    to be available during the execution of function [fn].
*)

val await :
  setup:(resolve:('a -> unit) -> reject:(exn -> unit) -> 'handle) ->
  cancel:('handle -> unit) ->
  'a
(** [await setup cancel] suspends the current fiber and calls
    [setup resolve reject]. The fiber is resumed normally with some
    value [v] when [resolve v] is called. It is resumed with exception
    [exn] when [reject exn] is called. If the fiber is cancelled,
    function [cancel] is called. *)

val set_uncaught_exception_handler :
  (exn -> Printexc.raw_backtrace -> unit) -> unit
(** [start fn] registers [fn] as the handler for uncaught exceptions. *)
