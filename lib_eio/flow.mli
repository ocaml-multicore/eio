(** Flows are used to represent byte streams, such as open files and network sockets.
    A {!source} provides a stream of bytes. A {!sink} consumes a stream.
    A {!two_way} can do both.

    To read structured data (e.g. a line at a time), wrap a source using {!Buf_read}. *)

open Std

(** {2 Types} *)

type source_ty = [`R | `Flow]
type 'a source = ([> source_ty] as 'a) r
(** A readable flow provides a stream of bytes. *)

type sink_ty = [`W | `Flow]
type 'a sink = ([> sink_ty] as 'a) r
(** A writeable flow accepts a stream of bytes. *)

type shutdown_ty = [`Shutdown]
type 'a shutdown = ([> shutdown_ty] as 'a) r

type 'a read_method = ..
(** Sources can offer a list of ways to read them, in order of preference. *)

type shutdown_command = [
  | `Receive  (** Indicate that no more reads will be done *)
  | `Send     (** Indicate that no more writes will be done *)
  | `All      (** Indicate that no more reads or writes will be done *)
]

(** {2 Reading} *)

val single_read : _ source -> Cstruct.t -> int
(** [single_read src buf] reads one or more bytes into [buf].

    It returns the number of bytes read (which may be less than the
    buffer size even if there is more data to be read).

    - Use {!read_exact} instead if you want to fill [buf] completely.
    - Use {!Buf_read.line} to read complete lines.
    - Use {!copy} to stream data directly from a source to a sink.

    [buf] must not be zero-length.

    @raise End_of_file if there is no more data to read *)

val read_exact : _ source -> Cstruct.t -> unit
(** [read_exact src dst] keeps reading into [dst] until it is full.
    @raise End_of_file if the buffer could not be filled. *)

val string_source : string -> source_ty r
(** [string_source s] is a source that gives the bytes of [s]. *)

val cstruct_source : Cstruct.t list -> source_ty r
(** [cstruct_source cs] is a source that gives the bytes of [cs]. *)

type 't read_method += Read_source_buffer of ('t -> (Cstruct.t list -> int) -> unit)
(** If a source offers [Read_source_buffer rsb] then the user can call [rsb t fn]
    to borrow a view of the source's buffers. [fn] returns the number of bytes it consumed.

    [rsb] will raise [End_of_file] if no more data will be produced.
    If no data is currently available, [rsb] will wait for some to become available before calling [fn].

    [fn] must not continue to use the buffers after it returns. *)

(** {2 Writing} *)

val write : _ sink -> Cstruct.t list -> unit
(** [write dst bufs] writes all bytes from [bufs].

    You should not perform multiple concurrent writes on the same flow
    (the output may get interleaved).

    This is a low level API. Consider using:

    - {!Buf_write} to combine multiple small writes.
    - {!copy} for bulk transfers, as it allows some extra optimizations. *)

val single_write : _ sink -> Cstruct.t list -> int
(** [single_write dst bufs] writes at least one byte from [bufs] and returns the number of bytes written. *)

val copy : _ source -> _ sink -> unit
(** [copy src dst] copies data from [src] to [dst] until end-of-file. *)

val copy_string : string -> _ sink -> unit
(** [copy_string s = copy (string_source s)] *)

val buffer_sink : Buffer.t -> sink_ty r
(** [buffer_sink b] is a sink that adds anything sent to it to [b].

    To collect data as a cstruct, use {!Buf_read} instead. *)

(** {2 Bidirectional streams} *)

type two_way_ty = [source_ty | sink_ty | shutdown_ty]
type 'a two_way = ([> two_way_ty] as 'a) r

val shutdown : _ two_way -> shutdown_command -> unit
(** [shutdown t cmd] indicates that the caller has finished reading or writing [t]
    (depending on [cmd]).

    This is useful in some protocols to indicate that you have finished sending the request,
    and that the remote peer should now send the response. *)

(** {2 Closing}

    Flows are usually attached to switches and closed automatically when the switch
    finishes. However, it can be useful to close them sooner manually in some cases. *)

val close : [> `Close] r -> unit
(** Alias of {!Resource.close}. *)

(** {2 Provider Interface} *)

module Pi : sig
  module type SOURCE = sig
    type t
    val read_methods : t read_method list
    val single_read : t -> Cstruct.t -> int
  end

  module type SINK = sig
    type t

    val single_write : t -> Cstruct.t list -> int

    val copy : t -> src:_ source -> unit
    (** [copy t ~src] allows for optimising copy operations.

        If you have no optimisations, you can use {!Eio.Flow.Pi.simple_copy} to implement this using {!single_write}. *)
  end

  module type SHUTDOWN = sig
    type t
    val shutdown : t -> shutdown_command -> unit
  end

  val source : (module SOURCE with type t = 't) -> ('t, source_ty) Resource.handler
  val sink : (module SINK with type t = 't) -> ('t, sink_ty) Resource.handler
  val shutdown : (module SHUTDOWN with type t = 't) -> ('t, shutdown_ty) Resource.handler

  module type TWO_WAY = sig
    include SHUTDOWN
    include SOURCE with type t := t
    include SINK with type t := t
  end

  val two_way : (module TWO_WAY with type t = 't) -> ('t, two_way_ty) Resource.handler

  type (_, _, _) Resource.pi +=
    | Source : ('t, (module SOURCE with type t = 't), [> source_ty]) Resource.pi
    | Sink : ('t, (module SINK with type t = 't), [> sink_ty]) Resource.pi
    | Shutdown : ('t, (module SHUTDOWN with type t = 't), [> shutdown_ty]) Resource.pi

  val simple_copy : single_write:('t -> Cstruct.t list -> int) -> 't -> src:_ source -> unit
  (** [simple_copy ~single_write] implements {!SINK}'s [copy] API using [single_write]. *)
end

