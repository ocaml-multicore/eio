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

type dir_fd =
  | Fd of fd    (** Confined to [fd]. *)
  | Cwd         (** Confined to "." *)
  | Fs          (** Unconfined "."; also allows absolute paths *)

val await_readable : string -> fd -> unit
val await_writable : string -> fd -> unit

val sleep_until : Mtime.t -> unit

val read : fd -> bytes -> int -> int -> int
val write : fd -> bytes -> int -> int -> int

val socket : sw:Switch.t -> Unix.socket_domain -> Unix.socket_type -> int -> fd
val connect : fd -> Unix.sockaddr -> unit
val accept : sw:Switch.t -> fd -> fd * Unix.sockaddr

val shutdown : fd -> Unix.shutdown_command -> unit

val recv_msg : fd -> Cstruct.t array -> Unix.sockaddr * int
val recv_msg_with_fds : sw:Switch.t -> max_fds:int -> fd -> Cstruct.t array -> Unix.sockaddr * int * fd list

val send_msg : fd -> ?fds:fd list -> ?dst:Unix.sockaddr -> Cstruct.t array -> int

val getrandom : Cstruct.t -> unit

val lseek : fd -> Optint.Int63.t -> [`Set | `Cur | `End] -> Optint.Int63.t
val fsync : fd -> unit
val ftruncate : fd -> Optint.Int63.t -> unit

(** Device numbers *)
module Dev : sig
  type t
  (** A device number, combining a major and a minor number.
      These are not portable across operating systems. *)

  val make : major:int -> minor:int -> t
  (** [make ~major ~minor] combines the two numbers (see [makedev(3)]).

      @raise Invalid_argument if either number is negative, or is too large
        to be represented on this platform. *)

  val major : t -> int
  (** [major t] is the major number of [t]. *)

  val minor : t -> int
  (** [minor t] is the minor number of [t]. *)

  val to_int64 : t -> int64
  (** [to_int64 t] is the raw [dev_t] value of [t], as the OS represents it. *)

  val pp : t Fmt.t
  (** [pp] formats a device number as [major:minor]. *)
end

type stat

val create_stat : unit -> stat

val fstat : buf:stat -> fd -> unit
val fstatat : buf:stat -> follow:bool -> dir_fd -> string -> unit

external blksize : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_blksize_bytes" "ocaml_eio_posix_stat_blksize_native" [@@noalloc]
external blocks  : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_blocks_bytes" "ocaml_eio_posix_stat_blocks_native" [@@noalloc]
external nlink   : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_nlink_bytes" "ocaml_eio_posix_stat_nlink_native" [@@noalloc]
external uid     : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_uid_bytes" "ocaml_eio_posix_stat_uid_native" [@@noalloc]
external gid     : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_gid_bytes" "ocaml_eio_posix_stat_gid_native" [@@noalloc]
external ino     : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_ino_bytes" "ocaml_eio_posix_stat_ino_native" [@@noalloc]
external size    : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_size_bytes" "ocaml_eio_posix_stat_size_native" [@@noalloc]

val rdev : stat -> Dev.t
(** [rdev t] is the device number this node refers to, if it is a device. *)

val dev : stat -> Dev.t
(** [dev t] is the device containing the filesystem the file resides on. *)

external perm    : stat -> (int [@untagged]) = "ocaml_eio_posix_stat_perm_bytes" "ocaml_eio_posix_stat_perm_native" [@@noalloc]
external mode    : stat -> (int [@untagged]) = "ocaml_eio_posix_stat_mode_bytes" "ocaml_eio_posix_stat_mode_native" [@@noalloc]
external kind    : stat -> Eio.File.Stat.kind = "ocaml_eio_posix_stat_kind"

external atime_sec : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_atime_sec_bytes" "ocaml_eio_posix_stat_atime_sec_native" [@@noalloc]
external ctime_sec : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_ctime_sec_bytes" "ocaml_eio_posix_stat_ctime_sec_native" [@@noalloc]
external mtime_sec : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_mtime_sec_bytes" "ocaml_eio_posix_stat_mtime_sec_native" [@@noalloc]

external atime_nsec : stat -> int = "ocaml_eio_posix_stat_atime_nsec" [@@noalloc]
external ctime_nsec : stat -> int = "ocaml_eio_posix_stat_ctime_nsec" [@@noalloc]
external mtime_nsec : stat -> int = "ocaml_eio_posix_stat_mtime_nsec" [@@noalloc]

val realpath : string -> string
val read_link : dir_fd -> string -> string

val mkdir : mode:int -> dir_fd -> string -> unit
val unlink : dir:bool -> dir_fd -> string -> unit
val rename : dir_fd -> string -> dir_fd -> string -> unit

val symlink : link_to:string -> dir_fd -> string -> unit
(** [symlink ~link_to dir path] will create a new symlink at [dir / path]
    linking to [link_to]. *)

type node_kind = [
  | `Fifo                   (** FIFO (named pipe). *)
  | `Sock                   (** Unix-domain socket. *)
  | `Reg                    (** Regular file. *)
  | `Chr of Dev.t           (** Character device with the given device number. *)
  | `Blk of Dev.t           (** Block device with the given device number. *)
]
(** The kind of node {!mknod} should create. *)

val mknod : node_kind -> perm:int -> dir_fd -> string -> unit
(** [mknod kind ~perm dir path] creates a fs node of type [kind] at [dir / path]. *)

val chmod : follow:bool -> mode:int -> dir_fd -> string -> unit

val chown : follow:bool -> ?uid:int64 -> ?gid:int64 -> dir_fd -> string -> unit
(** [chown ~follow ~uid ~gid dir path] will change the ownership of [dir / path]
    to [uid, gid]. *)

val readdir : dir_fd -> string -> string array
val with_dir_entries : dir_fd -> string -> ((Eio.File.Stat.kind * string) Seq.t -> 'a) -> 'a

val readv : fd -> Cstruct.t array -> int
val writev : fd -> Cstruct.t array -> int

val preadv : file_offset:Optint.Int63.t -> fd -> Cstruct.t array -> int
val pwritev : file_offset:Optint.Int63.t -> fd -> Cstruct.t array -> int

val pipe : sw:Switch.t -> fd * fd

module Open_flags : sig
  type t

  val rdonly : t
  val rdwr : t
  val wronly : t
  val append : t
  val creat : t
  val directory : t
  val dsync : t option
  val excl : t
  val noctty : t
  val nofollow : t
  val sync : t
  val trunc : t
  val resolve_beneath : t option
  val path : t option

  val empty : t
  val ( + ) : t -> t -> t
  val ( +? ) : t -> t option -> t       (** Add if available *)
end

val openat : sw:Switch.t -> mode:int -> dir_fd -> string -> Open_flags.t -> fd
(** Note: the returned FD is always non-blocking and close-on-exec. *)

module Process : sig
  type t
  (** A child process. *)

  module Fork_action = Eio_unix.Private.Fork_action
  (** Setup actions to perform in the child process. *)

  val spawn : sw:Switch.t -> Fork_action.t list -> t
  (** [spawn ~sw actions] forks a child process, which executes [actions].
      The last action should be {!Fork_action.execve}.

      You will typically want to do [Promise.await (exit_status child)] after this.

      @param sw The child will be sent {!Sys.sigkill} if [sw] finishes. *)

  val signal : t -> int -> unit
  (** [signal t x] sends signal [x] to [t].

      This is similar to doing [Unix.kill t.pid x],
      except that it ensures no signal is sent after [t] has been reaped. *)

  val pid : t -> int

  val exit_status : t -> Unix.process_status Promise.t
  (** [exit_status t] is a promise for the process's exit status. *)
end

(**/**)
(* Exposed for testing only. *)
module Resolve : sig
  val open_beneath_fallback : ?dirfd:Unix.file_descr -> sw:Switch.t -> mode:int -> string -> Open_flags.t -> fd
  val open_unconfined : sw:Switch.t -> mode:int -> fd option -> string -> Open_flags.t -> fd
end
(**/**)
