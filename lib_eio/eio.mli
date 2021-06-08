open Fibreslib

(** A base class for objects that can be queried at runtime for extra features. *)
module Generic : sig
  type 'a ty = ..
  (** An ['a ty] is a query for a feature of type ['a]. *)

  class type t = object
    method probe : 'a. 'a ty -> 'a option
  end

  val probe : #t -> 'a ty -> 'a option
end

(** Byte streams. *)
module Flow : sig
  class type close = object
    method close : unit
  end

  val close : #close -> unit

  class virtual read : object
    method virtual read_into : ?sw:Switch.t -> Cstruct.t -> int
  end

  val read_into : ?sw:Switch.t -> #read -> Cstruct.t -> int
  (** [read_into buf] reads one or more bytes into [buf].
      It returns the number of bytes written (which may be less than the
      buffer size even if there is more data to be read).
      [buf] must not be zero-length.
      @param sw Abort the read if [sw] is turned off.
      @raise End_of_file if there is no more data to read *)

  (** Producer base class. *)
  class virtual source : object
    inherit Generic.t
    inherit read
  end

  val string_source : string -> source

  val cstruct_source : Cstruct.t list -> source

  class virtual write : object
    method virtual write : 'a. ?sw:Switch.t -> (#source as 'a) -> unit
  end

  val copy : ?sw:Switch.t -> #source -> #write -> unit
  (** [copy src dst] copies data from [src] to [dst] until end-of-file. *)

  val copy_string : ?sw:Switch.t -> string -> #write -> unit

  (** Consumer base class. *)
  class virtual sink : object
    inherit Generic.t
    inherit write
  end

  val buffer_sink : Buffer.t -> sink

  (** Bidirectional stream base class. *)
  class virtual two_way : object
    inherit Generic.t
    inherit read
    inherit write

    method virtual shutdown : Unix.shutdown_command -> unit
  end

  val shutdown : #two_way -> Unix.shutdown_command -> unit
end

module Network : sig
  module Sockaddr : sig
    type t = Unix.sockaddr

    val pp : Format.formatter -> t -> unit
  end

  module Listening_socket : sig
    class virtual t : object
      method virtual close : unit
      method virtual listen : int -> unit
      method virtual accept_sub :
        sw:Switch.t ->
        on_error:(exn -> unit) ->
        (sw:Switch.t -> <Flow.two_way; Flow.close> -> Sockaddr.t -> unit) ->
        unit
    end

    val listen : #t -> int -> unit

    val accept_sub :
      sw:Switch.t ->
      #t ->
      on_error:(exn -> unit) ->
      (sw:Switch.t -> <Flow.two_way; Flow.close> -> Sockaddr.t -> unit) ->
      unit
    (** [accept t fn] waits for a new connection to [t] and then runs [fn ~sw flow client_addr] in a new fibre,
        created with [Fibre.fork_sub_ignore].
        [flow] will be closed automatically when the sub-switch is finished, if not already closed by then. *)
  end

  class virtual t : object
    method virtual bind : reuse_addr:bool -> sw:Switch.t -> Sockaddr.t -> Listening_socket.t
    method virtual connect : sw:Switch.t -> Sockaddr.t -> <Flow.two_way; Flow.close>
  end

  val bind : ?reuse_addr:bool -> sw:Switch.t -> #t -> Sockaddr.t -> Listening_socket.t
  (** [bind ~sw t addr] is a new listening socket bound to local address [addr].
      The new socket will be closed when [sw] finishes, unless closed manually first. *)

  val connect : sw:Switch.t -> #t -> Sockaddr.t -> <Flow.two_way; Flow.close>
  (** [connect ~sw t addr] is a new socket connected to remote address [addr].
      The new socket will be closed when [sw] finishes, unless closed manually first. *)
end

(** The standard environment of a process. *)
module Stdenv : sig
  type t = <
    stdin  : Flow.source;
    stdout : Flow.sink;
    stderr : Flow.sink;
    network : Network.t;
  >

  val stdin  : <stdin  : #Flow.source as 'a; ..> -> 'a
  val stdout : <stdout : #Flow.sink   as 'a; ..> -> 'a
  val stderr : <stderr : #Flow.sink   as 'a; ..> -> 'a

  val network : <network : #Network.t as 'a; ..> -> 'a
end
