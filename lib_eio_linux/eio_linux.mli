(*
 * Copyright (C) 2020-2021 Anil Madhavapeddy
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Eio.Std

type t

(** Wrap [Unix.file_descr] to track whether it has been closed. *)
module FD : sig
  type t

  val is_open : t -> bool
  (** [is_open t] is [true] if {!close t} hasn't been called yet. *)

  val close : t -> unit
  (** [close t] closes [t].
      @raise Invalid_arg if [t] is already closed. *)

  val of_unix : sw:Switch.t -> seekable:bool -> Unix.file_descr -> t
  (** [of_unix ~sw ~seekable fd] wraps [fd] as an open file descriptor.
      This is unsafe if [fd] is closed directly (before or after wrapping it).
      @param sw The FD is closed when [sw] is released, if not closed manually first.
      @param seekable If true, we pass [-1] as the file offset, to use the current offset.
                      If false, pass [0] as the file offset, which is needed for sockets. *)

  val to_unix : t -> Unix.file_descr
  (** [to_unix t] returns the wrapped descriptor.
      This allows unsafe access to the FD.
      @raise Invalid_arg if [t] is closed. *)
end

val noop : unit -> unit
(** [noop ()] performs a uring noop. This is only useful for benchmarking. *)

(** {1 Time functions} *)

val sleep_until : float -> unit
(** [sleep_until time] blocks until the current time is [time].
    @param sw Cancel the sleep if [sw] is turned off. *)

(** {1 Memory allocation functions} *)

val alloc : unit -> Uring.Region.chunk

val free : Uring.Region.chunk -> unit

val with_chunk : (Uring.Region.chunk -> 'a) -> 'a
(** [with_chunk fn] runs [fn chunk] with a freshly allocated chunk and then frees it. *)

(** {1 File manipulation functions} *)

val openfile : sw:Switch.t -> string -> Unix.open_flag list -> int -> FD.t
(** Like {!Unix.open_file}. *)

val openat2 :
  sw:Switch.t ->
  ?seekable:bool ->
  access:[`R|`W|`RW] ->
  flags:Uring.Open_flags.t ->
  perm:Unix.file_perm ->
  resolve:Uring.Resolve.t ->
  ?dir:FD.t -> string -> FD.t
(** [openat2 ~sw ~flags ~perm ~resolve ~dir path] opens [dir/path].
    See {!Uring.openat2} for details. *)

val read_upto : ?file_offset:Optint.Int63.t -> FD.t -> Uring.Region.chunk -> int -> int
(** [read_upto fd chunk len] reads at most [len] bytes from [fd],
    returning as soon as some data is available.
    @param file_offset Read from the given position in [fd] (default: 0).
    @raise End_of_file Raised if all data has already been read. *)

val read_exactly : ?file_offset:Optint.Int63.t -> FD.t -> Uring.Region.chunk -> int -> unit
(** [read_exactly fd chunk len] reads exactly [len] bytes from [fd],
    performing multiple read operations if necessary.
    @param file_offset Read from the given position in [fd] (default: 0).
    @raise End_of_file Raised if the stream ends before [len] bytes have been read. *)

val readv : ?file_offset:Optint.Int63.t -> FD.t -> Cstruct.t list -> int
(** [readv] is like {!read_upto} but can read into any cstruct(s),
    not just chunks of the pre-shared buffer.
    If multiple buffers are given, they are filled in order. *)

val write : ?file_offset:Optint.Int63.t -> FD.t -> Uring.Region.chunk -> int -> unit
(** [write fd buf len] writes exactly [len] bytes from [buf] to [fd].
    It blocks until the OS confirms the write is done,
    and resubmits automatically if the OS doesn't write all of it at once. *)

val writev : ?file_offset:Optint.Int63.t -> FD.t -> Cstruct.t list -> unit
(** [writev] is like {!write} but can write from any cstruct(s),
    not just chunks of the pre-shared buffer.
    If multiple buffers are given, they are sent in order.
    It will make multiple OS calls if the OS doesn't write all of it at once. *)

val splice : FD.t -> dst:FD.t -> len:int -> int
(** [splice src ~dst ~len] attempts to copy up to [len] bytes of data from [src] to [dst].
    @return The number of bytes copied.
    @raise End_of_file [src] is at the end of the file.
    @raise Unix.Unix_error(EINVAL, "splice", _) if splice is not supported for these FDs. *)

val connect : FD.t -> Unix.sockaddr -> unit
(** [connect fd addr] attempts to connect socket [fd] to [addr]. *)

val await_readable : FD.t -> unit
(** [await_readable fd] blocks until [fd] is readable (or has an error). *)

val await_writable : FD.t -> unit
(** [await_writable fd] blocks until [fd] is writable (or has an error). *)

val fstat : FD.t -> Unix.stats
(** Like {!Unix.fstat}. *)

(** {1 Sockets} *)

val accept : sw:Switch.t -> FD.t -> (FD.t * Unix.sockaddr)
(** [accept ~sw t] blocks until a new connection is received on listening socket [t].
    It returns the new connection and the address of the connecting peer.
    The new connection has the close-on-exec flag set automatically.
    The new connection is attached to [sw] and will be closed when that finishes, if
    not already closed manually by then. *)

val shutdown : FD.t -> Unix.shutdown_command -> unit
(** Like {!Unix.shutdown}. *)

(** {1 Eio API} *)

module Objects : sig
  type has_fd = < fd : FD.t >
  type source = < Eio.Flow.source; Eio.Flow.close; has_fd >
  type sink   = < Eio.Flow.sink  ; Eio.Flow.close; has_fd >

  type stdenv = <
    stdin  : source;
    stdout : sink;
    stderr : sink;
    net : Eio.Net.t;
    domain_mgr : Eio.Domain_manager.t;
    clock : Eio.Time.clock;
    fs : Eio.Dir.t;
    cwd : Eio.Dir.t;
  >

  val get_fd : <has_fd; ..> -> FD.t
  val get_fd_opt : #Eio.Generic.t -> FD.t option
end

val pipe : Switch.t -> Objects.source * Objects.sink
(** [pipe sw] is a source-sink pair [(r, w)], where data written to [w] can be read from [r].
    It is implemented as a Unix pipe. *)

(** {1 Main Loop} *)

val run : ?queue_depth:int -> ?block_size:int -> (Objects.stdenv -> unit) -> unit
(** FIXME queue_depth and block_size should be in a handler and not the mainloop *)
