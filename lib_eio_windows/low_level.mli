(** This module provides an effects-based API for calling POSIX functions.

    Normally it's better to use the cross-platform {!Eio} APIs instead,
    which uses these functions automatically where appropriate.

    These functions mostly copy the POSIX APIs directly, except that:

    + They suspend the calling fiber instead of returning [EAGAIN] or similar.
    + They handle [EINTR] by automatically restarting the call.
    + They wrap {!Unix.file_descr} in {!Fd}, to avoid use-after-close bugs.
    + They attach new FDs to switches, to avoid resource leaks. *)

open Eio.Std

type fd := Eio_unix.Fd.t

val await_readable : fd -> unit
val await_writable : fd -> unit

val sleep_until : Mtime.t -> unit

val read : fd -> bytes -> int -> int -> int
val read_cstruct : fd -> Cstruct.t -> int
val write : fd -> bytes -> int -> int -> int
val write_cstruct : fd -> Cstruct.t -> int

val socket : sw:Switch.t -> Unix.socket_domain -> Unix.socket_type -> int -> fd
val connect : fd -> Unix.sockaddr -> unit
val accept : sw:Switch.t -> fd -> fd * Unix.sockaddr

val shutdown : fd -> Unix.shutdown_command -> unit

val recv_msg : fd -> bytes -> int * Unix.sockaddr
val send_msg : fd -> ?dst:Unix.sockaddr -> bytes -> int

val getrandom : Cstruct.t -> unit

val lseek : fd -> Optint.Int63.t -> [`Set | `Cur | `End] -> Optint.Int63.t
val fsync : fd -> unit
val ftruncate : fd -> Optint.Int63.t -> unit

type follow = Follow | Nofollow | Open_link

val fstat : fd -> Unix.LargeFile.stats
val lstat : string -> Unix.LargeFile.stats

val full_path : string -> string
(** Make a path absolute using Windows' current directories, without accessing its target. *)

val realpath : string -> string

val realpath_fd : fd -> string
(** The current absolute path of the open object. This is for display and
    containment comparisons, not for reopening it: the path can acquire reparse points. *)

val read_link : ?dirfd:fd -> string -> string
(** [read_link ?dirfd path] is the target of the symlink or junction [path]. *)

val reparse_target : fd -> Eio_utils.Nt_path.link_target option
(** [reparse_target fd] is the raw NT target and relative/absolute flag of the
    symlink or junction open on [fd], or [None] if it is not one.
    Unlike {!read_link}, this does not convert the target to a display path. *)

val chown : ?dirfd:fd -> follow:bool -> ?uid:int64 -> ?gid:int64 -> string -> unit

val mkdir : ?dirfd:fd -> ?follow:follow -> mode:int -> string -> unit
val unlink : ?dirfd:fd -> dir:bool -> string -> unit

val rename : ?old_dir:fd -> string -> ?new_dir:fd -> string -> unit
(** [rename ?old_dir old ?new_dir new] moves [old] to [new], replacing an
    existing file or empty directory. Some volumes such as FAT cannot
    replace a directory and will fail. *)

val symlink_target : string -> Eio_utils.Nt_path.link_target
(** Convert a Win32 target once, for both type detection and creation. *)

val symlink_raw : to_dir:bool -> target:Eio_utils.Nt_path.link_target -> fd option -> string -> unit
(** Create a link with an already converted target. *)

val symlink : ?to_dir:bool -> link_to:string -> fd option -> string -> unit
(** [symlink ~link_to dir path] will create a new symlink at [dir / path]
    linking to [link_to]. With [dir], creation uses the directory handle throughout.

    @param to_dir Windows links are either to files or to directories.
                  If [true] this is a link to a directory (default [false]). *)

val chmod : mode:int -> fd option -> string -> unit
(** [chmod ~mode path] is just a non-blocking call to {! Unix.chmod} when
    [fd = None], otherwise it is unsupported. *)

val readdir : string -> string array

val readdir_fd : fd -> string array
(** List the open directory itself, even if it has since become a reparse point.
    The handle must have read access. Each call restarts the enumeration. *)

val readv : fd -> Cstruct.t array -> int
val writev : fd -> Cstruct.t list -> unit

val preadv : file_offset:Optint.Int63.t -> fd -> Cstruct.t array -> int
val pwritev : file_offset:Optint.Int63.t -> fd -> Cstruct.t array -> int

val pipe : sw:Switch.t -> fd * fd

module Flags : sig
  module Open : sig
    type t

    val rdonly : t
    val rdwr : t
    val wronly : t
    val creat : t
    val excl : t
    val trunc : t

    val generic_read : t
    val generic_write : t
    val synchronise : t
    val append : t

    val empty : t
    val ( + ) : t -> t -> t
  end

  module Disposition : sig
    type t

    val supersede : t
    (** If the file already exists, replace it with the given file.
        If it does not, create the given file. *)

    val create : t
    (** Create the file, if it already exists fail. *)

    val open_ : t
    (** If the file already exists, open it otherwise fail. *)

    val open_if : t
    (** If the file already exists, open it otherwise create it. *)

    val overwrite : t
    (** If the file already exists, open it and overwrite it otherwise fail. *)

    val overwrite_if : t
    (** If the file already exists, open it and overwrite it otherwise create it. *)
  end

  module Create : sig
    type t

    val empty : t
    (** No create options: allow both directories and non-directories. *)

    val directory : t
    (** Create a directory. *)
    
    val non_directory : t
    (** Create something that is not a directory. *)

    val no_intermediate_buffering : t

    val write_through : t

    val sequential_only : t

    val ( + ) : t -> t -> t
  end
end

val openat : ?dirfd:fd -> ?beneath:bool -> ?follow:follow -> sw:Switch.t -> string -> Flags.Open.t -> Flags.Disposition.t -> Flags.Create.t -> fd
(** [openat ?dirfd ~sw path ...] opens [path], relative to [dirfd] if given
    and otherwise a Win32 path, relative to the current directory.

    @param beneath If [true], fail with [ELOOP] rather than follow any link on
                   the way to [path], including at the leaf unless [follow = Open_link].
                   [path] cannot use [".."] relative to [dirfd], so this keeps it beneath [dirfd].

    @param follow [Nofollow] fails with [ELOOP] if the leaf is a link, but follows links before it.

    Note: the returned FD is always non-blocking and close-on-exec. *)
