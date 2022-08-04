(** Flows are used to represent byte streams, such as open files and network sockets.
    A {!source} provides a stream of bytes. A {!sink} consumes a stream.
    A {!two_way} can do both.

    To read structured data (e.g. a line at a time), wrap a source using {!Buf_read}. *)

(** {2 Reading} *)

type read_method = ..
(** Sources can offer a list of ways to read them, in order of preference. *)

class virtual source : object
  inherit Generic.t
  method read_methods : read_method list
  method virtual read_into : Cstruct.t -> int
end

val read : #source -> Cstruct.t -> int
(** [read src buf] reads one or more bytes into [buf].

    It returns the number of bytes read (which may be less than the
    buffer size even if there is more data to be read).
    [read src] just makes a single call to [src#read_into]
    (and asserts that the result is in range).

    - Use {!read_exact} instead if you want to fill [buf] completely.
    - Use {!Buf_read.line} to read complete lines.
    - Use {!copy} to stream data directly from a source to a sink.

    [buf] must not be zero-length.

    @raise End_of_file if there is no more data to read *)

val read_exact : #source -> Cstruct.t -> unit
(** [read_exact src dst] keeps reading into [dst] until it is full.
    @raise End_of_file if the buffer could not be filled. *)

val read_methods : #source -> read_method list
(** [read_methods flow] is a list of extra ways of reading from [flow],
    with the preferred (most efficient) methods first.

    If no method is suitable, {!read} should be used as the fallback. *)

val string_source : string -> source
(** [string_source s] is a source that gives the bytes of [s]. *)

val cstruct_source : Cstruct.t list -> source
(** [cstruct_source cs] is a source that gives the bytes of [cs]. *)

type read_method += Read_source_buffer of ((Cstruct.t list -> int) -> unit)
(** If a source offers [Read_source_buffer rsb] then the user can call [rsb fn]
    to borrow a view of the source's buffers. [fn] returns the number of bytes it consumed.

    [rsb] will raise [End_of_file] if no more data will be produced.
    If no data is currently available, [rsb] will wait for some to become available before calling [fn].

    [fn] must not continue to use the buffers after it returns. *)

(** {2 Writing} *)

(** Consumer base class. *)
class virtual sink : object
  inherit Generic.t
  method virtual copy : 'a. (#source as 'a) -> unit
end

val copy : #source -> #sink -> unit
(** [copy src dst] copies data from [src] to [dst] until end-of-file. *)

val copy_string : string -> #sink -> unit
(** [copy_string s = copy (string_source s)] *)

val buffer_sink : Buffer.t -> sink
(** [buffer_sink b] is a sink that adds anything sent to it to [b].

    To collect data as a cstruct, use {!Buf_read} instead. *)

(** {2 Bidirectional streams} *)

type shutdown_command = [
  | `Receive  (** Indicate that no more reads will be done *)
  | `Send     (** Indicate that no more writes will be done *)
  | `All      (** Indicate that no more reads or writes will be done *)
]

class virtual two_way : object
  inherit source
  inherit sink

  method virtual shutdown : shutdown_command -> unit
end

val shutdown : #two_way -> shutdown_command -> unit
(** [shutdown t cmd] indicates that the caller has finished reading or writing [t]
    (depending on [cmd]).

    This is useful in some protocols to indicate that you have finished sending the request,
    and that the remote peer should now send the response. *)

(** {2 Closing}

    Flows are usually attached to switches and closed automatically when the switch
    finishes. However, it can be useful to close them sooner manually in some cases. *)

class type close = object
  method close : unit
end

val close : #close -> unit
(** [close t] marks the flow as closed. It can no longer be used after this. *)
