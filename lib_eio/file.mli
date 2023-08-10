open Std

(** Traditional Unix permissions. *)
module Unix_perm : sig
  type t = int
  (** This is the same as {!Unix.file_perm}, but avoids a dependency on [Unix]. *)
end

(** Portable file stats. *)
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
(** Pretty printer for {! kind}. *)

type 'a stat =
  | Dev : int64 stat
  | Ino : int64 stat
  | Kind : kind stat
  | Perm : int stat
  | Nlink : int64 stat
  | Uid : int64 stat
  | Gid : int64 stat
  | Rdev : int64 stat
  | Size : int64 stat
  | Atime : float stat
  | Ctime : float stat
  | Mtime : float stat

type ('a, 'ty) stats =
  | [] : ('ty, 'ty) stats
  | (::) : 'a stat * ('b, 'ty) stats -> ('a -> 'b, 'ty) stats

type ro_ty = [`File | Flow.source_ty | Resource.close_ty]

type 'a ro = ([> ro_ty] as 'a) r
(** A file opened for reading. *)

type rw_ty = [ro_ty | Flow.sink_ty]

type 'a rw = ([> rw_ty] as 'a) r
(** A file opened for reading and writing. *)

val kind : _ ro -> kind
(** [kind t] returns the kind of file that [t] is.
    Equivalent to [stat t [Kind] Fun.id],
    so use {!val:stat} to query multiple items from a file in one call. *)

val size : _ ro -> Optint.Int63.t
(** [size t] returns the size of [t]. Equivalent to [stat t [File.size] Fun.id],
    so use {!val:stat} to query multiple items from a file in one call. *)

val perm : _ ro -> int
(** [perm t] returns the file permissions of [t].
    Equivalent to [stat t [Perm] Fun.id],
    so use {!val:stat} to query multiple items from a file in one call. *)

val uid : _ ro -> int64
(** [uid t] returns the user id associated with [t].
    Equivalent to [stat t [Uid] Fun.id],
    so use {!val:stat} to query multiple items from a file in one call. *)

val gid : _ ro -> int64
(** [gid t] returns the group id associated with [t].
    Equivalent to [stat t [Gid] Fun.id],
    so use {!val:stat} to query multiple items from a file in one call. *)

val atime : _ ro -> float
(** [atime t] returns the last access time of [t] as the seconds
    since the start of the epoch.
    Equivalent to [stat t [Atime] Fun.id],
    so use {!val:stat} to query multiple items from a file in one call. *)

val mtime : _ ro -> float
(** [mtime t] returns the last modified time of [t] as the seconds
    since the start of the epoch.
    Equivalent to [stat t [Mtime] Fun.id],
    so use {!val:stat} to query multiple items from a file in one call. *)

val ctime : _ ro -> float
(** [ctime t] returns the creation time of [t] as the seconds
    since the start of the epoch.
    Equivalent to [stat t [Ctime] Fun.id],
    so use {!val:stat} to query multiple items from a file in one call. *)

val stat : _ ro -> ('a, 'b) stats -> 'a -> 'b
(** [stat t fields fn] will retrieve the file statistics for the specified
    [fields] and apply them as arguments to [fn].

    For example, to print the kind of a file along with its size and last
    modified time:

    {[ stat t [ Kind; Size; Mtime ]
        (fun kind size mtime ->
          traceln "kind: %a size: %Ld mtime: %f" pp_kind size mtime)
     ]}

    If you only require access to one field, consider using simpler accessor
    functions in this module such as {!val:size} or {!val:kind}.  *)

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

module Pi : sig
  module type READ = sig
    include Flow.Pi.SOURCE

    val pread : t -> file_offset:Optint.Int63.t -> Cstruct.t list -> int
    val stat : 'a 'b . t -> ('a, 'b) stats -> 'a -> 'b
    val close : t -> unit
  end

  module type WRITE = sig
    include Flow.Pi.SINK
    include READ with type t := t

    val pwrite : t -> file_offset:Optint.Int63.t -> Cstruct.t list -> int
  end

  type (_, _, _) Resource.pi +=
    | Read : ('t, (module READ with type t = 't), [> ro_ty]) Resource.pi
    | Write : ('t, (module WRITE with type t = 't), [> rw_ty]) Resource.pi

  val ro : (module READ with type t = 't) -> ('t, ro_ty) Resource.handler

  val rw : (module WRITE with type t = 't) -> ('t, rw_ty) Resource.handler
end
