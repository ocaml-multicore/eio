(** Eio backend using libuv.

    You will normally not use this module directly.
    Instead, use {!Eio_main.run} to start an event loop and then use the API in the {!Eio} module.

    However, it is possible to use this module directly if you only want to support libuv. *)

open Eio.Std

module Low_level : sig
  type 'a or_error = ('a, Luv.Error.t) result

  exception Luv_error of Luv.Error.t

  val or_raise : 'a or_error -> 'a
  (** [or_raise (Error e)] raises [Luv_error e]. *)

  val await_with_cancel :
    request:[< `File | `Addr_info | `Name_info | `Random | `Thread_pool ] Luv.Request.t ->
    (Luv.Loop.t -> ('a -> unit) -> unit) -> 'a
  (** [await_with_cancel ~request fn] converts a function using a luv-style callback to one using effects.
      It sets the fiber's cancel function to cancel [request], and clears it when the operation completes. *)

  (** {1 Time functions} *)

  val sleep_until : float -> unit
  (** [sleep_until time] blocks until the current time is [time]. *)

  (** {1 DNS functions} *)

  val getaddrinfo : service:string -> string -> Eio.Net.Sockaddr.t list
  (** [getaddrinfo ~service host] returns a list of IP addresses for [host]. [host] is either a domain name or
      an ipaddress. *)

  (** {1 Low-level wrappers for Luv functions} *)

  module File : sig
    type t

    val is_open : t -> bool
    (** [is_open t] is [true] if {!close} hasn't been called yet. *)

    val close : t -> unit
    (** [close t] closes [t].
        @raise Invalid_arg if [t] is already closed. *)

    val of_luv : ?close_unix:bool -> sw:Switch.t -> Luv.File.t -> t
    (** [of_luv ~sw fd] wraps [fd] as an open file descriptor.
        This is unsafe if [fd] is closed directly (before or after wrapping it).
        @param sw The FD is closed when [sw] is released, if not closed manually first.
        @param close_unix if [true] (the default), calling [close] also closes [fd]. *)

    val to_luv : t -> Luv.File.t
    (** [to_luv t] returns the wrapped descriptor.
        This allows unsafe access to the FD.
        @raise Invalid_arg if [t] is closed. *)

    val fstat : t -> Luv.File.Stat.t or_error
    (** [fstat fd] returns the stat of [fd]. *)

    val open_ :
      sw:Switch.t ->
      ?mode:Luv.File.Mode.t list ->
      string -> Luv.File.Open_flag.t list -> t or_error
    (** Wraps {!Luv.File.open_} *)

    val read : ?file_offset:int64 -> t -> Luv.Buffer.t list -> Unsigned.Size_t.t or_error
    (** Wraps {!Luv.File.read} *)

    val write_single : ?file_offset:int64 -> t -> Luv.Buffer.t list -> Unsigned.Size_t.t or_error
    (** [write_single t bufs] performs a single write call and returns the number of bytes written,
        which may be less than the amount of data provided in [bufs]. *)

    val write : t -> Luv.Buffer.t list -> unit or_error
    (** [write t bufs] writes all the data in [bufs] (which may take several calls to {!write_single}). *)

    val realpath : string -> string or_error
    (** Wraps {!Luv.File.realpath} *)

    val mkdir : mode:Luv.File.Mode.t list -> string -> unit or_error
    (** Wraps {!Luv.File.mkdir} *)

    val rmdir : string -> unit or_error
    (** Wraps {!Luv.File.rmdir} *)

    val unlink : string -> unit or_error
    (** Wraps {!Luv.File.unlink} *)

    val readdir : string -> string list or_error
    (** Wraps {!Luv.File.readdir}. [readdir] opens and closes the directory for reading for the user. *)
  end

  module Random : sig
    val fill : Luv.Buffer.t -> unit
    (** Wraps {!Luv.Random.random} *)
  end

  module Handle : sig
    type 'a t
      constraint 'a = [< `Poll | `Stream of [< `Pipe | `TCP | `TTY ] | `UDP ]

    val is_open : 'a t -> bool
    (** [is_open t] is [true] if {!close} hasn't been called yet. *)

    val close : 'a t -> unit
    (** [close t] closes [t].
        @raise Invalid_arg if [t] is already closed. *)

    val to_luv : 'a t -> 'a Luv.Handle.t
    (** [to_luv t] returns the wrapped handle.
        This allows unsafe access to the handle.
        @raise Invalid_arg if [t] is closed. *)

    val of_luv : ?close_unix:bool -> sw:Switch.t -> 'a Luv.Handle.t -> 'a t
    (** [of_luv ~sw h] wraps [h] as an open handle.
        This is unsafe if [h] is closed directly (before or after wrapping it).
        @param sw The handle is closed when [sw] is released, if not closed manually first.
        @param close_unix if [true] (the default), calling [close] also closes [fd]. *)
  end
end

(** {1 Eio API} *)

type has_fd = < fd : Low_level.File.t >
type source = < Eio.Flow.source; Eio.Flow.close; has_fd >
type sink   = < Eio.Flow.sink  ; Eio.Flow.close; has_fd >

type stdenv = <
  stdin  : source;
  stdout : sink;
  stderr : sink;
  net : Eio.Net.t;
  domain_mgr : Eio.Domain_manager.t;
  clock : Eio.Time.clock;
  mono_clock : Eio.Time.Mono.t;
  fs : Eio.Fs.dir Eio.Path.t;
  cwd : Eio.Fs.dir Eio.Path.t;
  secure_random : Eio.Flow.source;
  debug : Eio.Debug.t;
>

val get_fd : <has_fd; ..> -> Low_level.File.t
val get_fd_opt : #Eio.Generic.t -> Low_level.File.t option

(** {1 Main Loop} *)

val run : (stdenv -> 'a) -> 'a
