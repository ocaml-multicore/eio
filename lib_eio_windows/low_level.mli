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

val socket : sw:Switch.t -> Unix.socket_domain -> Unix.socket_type -> int -> fd
val connect : fd -> Unix.sockaddr -> unit
val accept : sw:Switch.t -> fd -> fd * Unix.sockaddr

val shutdown : fd -> Unix.shutdown_command -> unit

val recv_msg : fd -> bytes -> int * Unix.sockaddr
val send_msg : fd -> dst:Unix.sockaddr -> bytes -> int

val getrandom : Cstruct.t -> unit

val fstat : fd -> Unix.LargeFile.stats
val lstat : string -> Unix.LargeFile.stats

val realpath : string -> string

val mkdir : ?dirfd:fd -> mode:int -> string -> unit
val unlink : ?dirfd:fd -> dir:bool -> string -> unit
val rename : ?old_dir:fd -> string -> ?new_dir:fd -> string -> unit

val readdir : string -> string array

val readv : fd -> Cstruct.t array -> int
val writev : fd -> Cstruct.t list -> unit

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
  val excl : t
  val trunc : t

  val empty : t
  val ( + ) : t -> t -> t
end

val openat : ?dirfd:fd -> sw:Switch.t -> mode:int -> string -> Open_flags.t -> fd
(** Note: the returned FD is always non-blocking and close-on-exec. *)
