(** Low-level Windows I/O for Eio fibers.

    Prefer the portable {!Eio} APIs. These functions suspend the calling fiber
    while I/O is pending. They wrap descriptors in {!Fd} to prevent use after
    close and attach new descriptors to switches.

    Sockets and overlapped handles use completion ports. Synchronous handles use
    worker threads. Explicit readiness waits and Unix-domain connects use the
    scheduler's readiness fallback. *)

open Eio.Std

type fd := Eio_unix.Fd.t

val await_readable : fd -> unit
(** Wait until the socket is readable. *)

val await_writable : fd -> unit
(** Wait until the socket is writable. *)

val sleep_until : Mtime.t -> unit

val read : fd -> bytes -> int -> int -> int
(** Read into the specified byte range. Return the byte count, or zero at end of input. *)

val read_cstruct : fd -> Cstruct.t -> int
(** Read into the buffer. Return the byte count, or zero at end of input. *)

val write : fd -> bytes -> int -> int -> int
(** Write from the specified byte range. Return the byte count; writes may be partial. *)

val write_cstruct : fd -> Cstruct.t -> int
(** Write from the buffer. Return the byte count; writes may be partial. *)

val socket : sw:Switch.t -> Unix.socket_domain -> Unix.socket_type -> int -> fd
val connect : fd -> Unix.sockaddr -> unit
(** Connect the socket. Internet sockets are bound automatically when needed. *)

val accept : sw:Switch.t -> fd -> fd * Unix.sockaddr
(** Accept a connection and attach its socket to [sw]. Return the socket and peer address. *)

val shutdown : fd -> Unix.shutdown_command -> unit
(** Shut down the socket. Receive shutdown also cancels pending receives. *)

val recv_msg : fd -> bytes -> int * Unix.sockaddr
(** Receive one datagram and return the number of bytes copied and its source address.
    Excess bytes are discarded.
    Raise [Unix.Unix_error (ESHUTDOWN, _, _)] if receive shutdown cancels the operation. *)

val send_msg : fd -> ?dst:Unix.sockaddr -> bytes -> int
(** Send a message to [dst], or to the connected peer if omitted. Return the byte count. *)

val getrandom : Cstruct.t -> unit

val lseek : fd -> Optint.Int63.t -> [`Set | `Cur | `End] -> Optint.Int63.t
(** Set the file position relative to the start, current position or end.
    Return the new position. Serialize with sequential I/O on overlapped files. *)

val fsync : fd -> unit
val ftruncate : fd -> Optint.Int63.t -> unit

type follow = Follow | Nofollow | Open_link

val fstat : fd -> Unix.LargeFile.stats
val lstat : string -> Unix.LargeFile.stats

val full_path : string -> string
(** Make a path absolute using Windows' current directories, without accessing its target.

    @since 1.7 *)

val realpath : string -> string

val realpath_fd : fd -> string
(** The current absolute path of the open object. This is for display and
    containment comparisons, not for reopening it: the path can acquire reparse points.

    @since 1.7 *)

val read_link : ?dirfd:fd -> string -> string
(** [read_link ?dirfd path] is the target of the symlink or junction [path]. *)

val reparse_target : fd -> Eio_utils.Nt_path.link_target option
(** [reparse_target fd] is the raw NT target and relative/absolute flag of the
    symlink or junction open on [fd], or [None] if it is not one.
    Unlike {!read_link}, this does not convert the target to a display path.

    @since 1.7 *)

val chown : ?dirfd:fd -> follow:bool -> ?uid:int64 -> ?gid:int64 -> string -> unit

val mkdir : ?dirfd:fd -> ?follow:follow -> mode:int -> string -> unit
val unlink : ?dirfd:fd -> dir:bool -> string -> unit

val rename : ?old_dir:fd -> string -> ?new_dir:fd -> string -> unit
(** [rename ?old_dir old ?new_dir new] moves [old] to [new], replacing an
    existing file or empty directory. Some volumes such as FAT cannot
    replace a directory and will fail. *)

val symlink_target : string -> Eio_utils.Nt_path.link_target
(** Convert a Win32 target once, for both type detection and creation.

    @since 1.7 *)

val symlink_raw : to_dir:bool -> target:Eio_utils.Nt_path.link_target -> fd option -> string -> unit
(** Create a link with an already converted target.

    @since 1.7 *)

val symlink : ?to_dir:bool -> link_to:string -> fd option -> string -> unit
(** [symlink ~link_to dir path] will create a new symlink at [dir / path]
    linking to [link_to]. With [dir], creation uses the directory handle throughout.

    @param to_dir Windows links are either to files or to directories.
                  If [true] this is a link to a directory (default [false]).
                  Since 1.7. *)

val chmod : mode:int -> fd option -> string -> unit
(** [chmod ~mode path] is just a non-blocking call to {! Unix.chmod} when
    [fd = None], otherwise it is unsupported. *)

val readdir : string -> string array

val readdir_fd : fd -> string array
(** List the open directory itself, even if it has since become a reparse point.
    The handle must have read access. Each call restarts the enumeration.

    @since 1.7 *)

val readv : fd -> Cstruct.t array -> int
(** Read into the first non-empty buffer. Return the byte count. *)

val writev : fd -> Cstruct.t list -> unit
(** Write all buffers in order, retrying partial writes. *)

val preadv : file_offset:Optint.Int63.t -> fd -> Cstruct.t array -> int
(** Read at [file_offset] into the first non-empty buffer without changing the file position. *)

val pwritev : file_offset:Optint.Int63.t -> fd -> Cstruct.t array -> int
(** Write at [file_offset] from the first non-empty buffer without changing the file position.
    Return the byte count; writes may be partial. *)

val pipe : sw:Switch.t -> fd * fd
(** Create an overlapped byte pipe attached to [sw]. Return its read and write ends. *)

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
                   Since 1.7.

    @param follow [Nofollow] fails with [ELOOP] if the leaf is a link, but follows links before it.

    Note: the returned FD is synchronous and close-on-exec. Eio runs blocking
    operations on worker threads to preserve interoperability with [Unix]. *)
