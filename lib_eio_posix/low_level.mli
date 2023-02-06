(** This module provides an effects-based API for calling POSIX functions.

    Normally it's better to use the cross-platform {!Eio} APIs instead,
    which uses these functions automatically where appropriate.

    These functions mostly copy the POSIX APIs directly, except that:

    + They suspend the calling fiber instead of returning [EAGAIN] or similar.
    + They handle [EINTR] by automatically restarting the call.
    + They wrap {!Unix.file_descr} in {!Fd}, to avoid use-after-close bugs.
    + They attach new FDs to switches, to avoid resource leaks. *)

open Eio.Std

module Fd = Fd

val await_readable : Fd.t -> unit
val await_writable : Fd.t -> unit

val sleep_until : Mtime.t -> unit

val read : Fd.t -> bytes -> int -> int -> int
val write : Fd.t -> bytes -> int -> int -> int

val socket : sw:Switch.t -> Unix.socket_domain -> Unix.socket_type -> int -> Fd.t
val connect : Fd.t -> Unix.sockaddr -> unit
val accept : sw:Switch.t -> Fd.t -> Fd.t * Unix.sockaddr

val shutdown : Fd.t -> Unix.shutdown_command -> unit

val recv_msg : Fd.t -> bytes -> int * Unix.sockaddr
val send_msg : Fd.t -> dst:Unix.sockaddr -> bytes -> int

val getrandom : Cstruct.t -> unit

val fstat : Fd.t -> Unix.LargeFile.stats
val lstat : string -> Unix.LargeFile.stats

val realpath : string -> string

val mkdir : ?dirfd:Fd.t -> mode:int -> string -> unit
val unlink : ?dirfd:Fd.t -> dir:bool -> string -> unit
val rename : ?old_dir:Fd.t -> string -> ?new_dir:Fd.t -> string -> unit

val readdir : string -> string array

val readv : Fd.t -> Cstruct.t array -> int
val writev : Fd.t -> Cstruct.t array -> int

val preadv : file_offset:Optint.Int63.t -> Fd.t -> Cstruct.t array -> int
val pwritev : file_offset:Optint.Int63.t -> Fd.t -> Cstruct.t array -> int

module Open_flags : sig
  type t

  val rdonly : t
  val rdwr : t
  val wronly : t
  val append : t
  val creat : t
  val directory : t
  val dsync : t
  val excl : t
  val noctty : t
  val nofollow : t
  val sync : t
  val trunc : t

  val empty : t
  val ( + ) : t -> t -> t
end

val openat : ?dirfd:Fd.t -> sw:Switch.t -> mode:int -> string -> Open_flags.t -> Fd.t
(** Note: the returned FD is always non-blocking and close-on-exec. *)
