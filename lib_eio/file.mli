(** Files implement the {!Flow} APIs, which can be used for reading and writing data.
    This module provides additonal file-specific operations, such as seeking within a file.

    To get an open file, use the functions in the {!Path} module. *)

open Std

(** {2 Types} *)

(** Traditional Unix permissions. *)
module Unix_perm : sig
  type t = int
  (** This is the same as {!Unix.file_perm}, but avoids a dependency on [Unix]. *)
end

(** Portable file stats. *)
module Stat : sig

  type kind = [
    | `Unknown
    | `Fifo
    | `Character_special
    | `Directory
    | `Block_device
    | `Regular_file
    | `Symbolic_link
    | `Socket
  ]
  (** Kind of file from st_mode. **)

  val pp_kind : kind Fmt.t
  (** Pretty printer for {!type-kind}. *)

  type t = {
    dev : Int64.t;              (** Device containing the filesystem where the file resides. *)
    ino : Int64.t;              (** Inode number. *)
    kind : kind;                (** File type. *)
    perm : Unix_perm.t;         (** Permissions (mode). *)
    nlink : Int64.t;            (** Number of hard links. *)
    uid : Int64.t;              (** User ID of owner. *)
    gid : Int64.t;              (** Group ID of owner. *)
    rdev : Int64.t;             (** Device's ID (if this is a device). *)
    size : Optint.Int63.t;      (** Total size in bytes. *)
    atime : float;              (** Last access time. *)
    mtime : float;              (** Last modification time. *)
    ctime : float;              (** Creation time. *)
  }
  (** Like stat(2). *)

  val pp : t Fmt.t
  (** Pretty printer for {! t}. *)
end

type ro_ty = [`File | Flow.source_ty | Resource.close_ty]

type 'a ro = ([> ro_ty] as 'a) r
(** A file opened for reading. *)

type rw_ty = [ro_ty | Flow.sink_ty]

type 'a rw = ([> rw_ty] as 'a) r
(** A file opened for reading and writing. *)

(** {2 Metadata} *)

val stat : _ ro -> Stat.t
(** [stat t] returns the {!Stat.t} record associated with [t]. *)

val size : _ ro -> Optint.Int63.t
(** [size t] returns the size of [t]. *)

(** {2 Reading and writing} *)

val pread : _ ro -> file_offset:Optint.Int63.t -> Cstruct.t list -> int
(** [pread t ~file_offset bufs] performs a single read of [t] at [file_offset] into [bufs].

    It returns the number of bytes read, which may be less than the space in [bufs],
    even if more bytes are available. Use {!pread_exact} instead if you require
    the buffer to be filled.

    To read at the current offset, use {!Flow.single_read} instead. *)

val pread_exact : _ ro -> file_offset:Optint.Int63.t -> Cstruct.t list -> unit
(** [pread_exact t ~file_offset bufs] reads from [t] into [bufs] until [bufs] is full.

    @raise End_of_file if the buffer could not be filled. *)

val pwrite_single : _ rw -> file_offset:Optint.Int63.t -> Cstruct.t list -> int
(** [pwrite_single t ~file_offset bufs] performs a single write operation, writing
    data from [bufs] to location [file_offset] in [t].

    It returns the number of bytes written, which may be less than the length of [bufs].
    In most cases, you will want to use {!pwrite_all} instead. *)

val pwrite_all : _ rw -> file_offset:Optint.Int63.t -> Cstruct.t list -> unit
(** [pwrite_all t ~file_offset bufs] writes all the data in [bufs] to location [file_offset] in [t]. *)

val seek : _ ro -> Optint.Int63.t -> [`Set | `Cur | `End] -> Optint.Int63.t
(** Set and/or get the current file position.

    Like {!Unix.lseek}. *)

val sync : _ rw -> unit
(** Flush file buffers to disk.

    Like {!Unix.fsync}. *)

val truncate : _ rw -> Optint.Int63.t -> unit
(** Set the length of a file.

    Like {!Unix.ftruncate}. *)

(** {2 Provider Interface} *)

module Pi : sig
  module type READ = sig
    include Flow.Pi.SOURCE

    val pread : t -> file_offset:Optint.Int63.t -> Cstruct.t list -> int
    val stat : t -> Stat.t
    val seek : t -> Optint.Int63.t -> [`Set | `Cur | `End] -> Optint.Int63.t
    val close : t -> unit
  end

  module type WRITE = sig
    include Flow.Pi.SINK
    include READ with type t := t

    val pwrite : t -> file_offset:Optint.Int63.t -> Cstruct.t list -> int
    val sync : t -> unit
    val truncate : t -> Optint.Int63.t -> unit
  end

  type (_, _, _) Resource.pi +=
    | Read : ('t, (module READ with type t = 't), [> ro_ty]) Resource.pi
    | Write : ('t, (module WRITE with type t = 't), [> rw_ty]) Resource.pi

  val ro : (module READ with type t = 't) -> ('t, ro_ty) Resource.handler

  val rw : (module WRITE with type t = 't) -> ('t, rw_ty) Resource.handler
end
