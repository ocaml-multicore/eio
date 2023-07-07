val timestamper : Eio.Private.Tracing.log_buffer -> int -> unit
(** Uses [Mtime_clock] to write timestamps. *)

val mmap_buffer : size:int -> string -> Eio.Private.Tracing.log_buffer
(** [mmap_buffer ~size path] initialises file [path] as an empty buffer for tracing. *)

val with_tracing : ?size:int -> string -> (unit -> 'a) -> 'a
(** [with_tracing path fn] is a convenience function that uses {!mmap_buffer} to create a log buffer,
    calls {!Tracing.Control.start} to start recording, runs [fn], and then stops recording. *)
