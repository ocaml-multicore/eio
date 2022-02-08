(** The default implementation of {!Eio.traceln}. *)

val mutex : Mutex.t
(** The mutex used to prevent two domains writing to stderr at once.

    This might be useful if you want to write to it directly yourself,
    e.g. for a log reporter. *)

val default_traceln :
    ?__POS__:string * int * int * int ->
    ('a, Format.formatter, unit, unit) format4 -> 'a
(** [default_traceln] is a suitable default implementation for {!Eio.Std.traceln}.

    It writes output to stderr, prefixing each line with a "+".
    If [__POS__] is given, it also displays the file and line number from that.
    It uses {!mutex} so that only one domain's output is written at a time. *)
