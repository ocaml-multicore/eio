(** Eio backend using Linux's io_uring.

    You will normally not use this module directly.
    Instead, use {!Eio_main.run} to start an event loop and then use the API in the {!Eio} module.

    However, it is possible to use this module directly if you only want to support recent versions of Linux. *)

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

type fd := Eio_unix.Fd.t

(** {1 Main Loop} *)

type stdenv = Eio_unix.Stdenv.base

val run :
  ?queue_depth:int ->
  ?n_blocks:int ->
  ?block_size:int ->
  ?polling_timeout:int ->
  ?fallback:([`Msg of string] -> 'a) ->
  (stdenv -> 'a) -> 'a
(** Run an event loop using io_uring.

    Uses {!Uring.create} to create the io_uring,
    and {!Uring.set_fixed_buffer} to set a [block_size * n_blocks] fixed buffer.

    Note that if Linux resource limits prevent the requested fixed buffer from being allocated
    then [run] will continue without one (and log a warning).

    For portable code, you should use {!Eio_main.run} instead, which will use this automatically
    if running on Linux with a recent-enough kernel version.

    @param fallback Call this instead if io_uring is not available for some reason.
                    The argument is a message describing the problem (for logging).
                    The default simply raises an exception. *)

(** {1 Low-level API} *)

(** Low-level API for using uring directly. *)
module Low_level : sig
  val noop : unit -> unit
  (** [noop ()] performs a uring noop. This is only useful for benchmarking. *)

  (** {1 Time functions} *)

  val sleep_until : Mtime.t -> unit
  (** [sleep_until time] blocks until the current time is [time]. *)

  (** {1 Fixed-buffer memory allocation functions}

      The size of the fixed buffer is set when calling {!run}, which attempts to allocate a fixed buffer.
      However, that may fail due to resource limits. *)

  val alloc_fixed : unit -> Uring.Region.chunk option
  (** Allocate a chunk of memory from the fixed buffer.

      Warning: The memory is NOT zeroed out.

      Passing such memory to Linux can be faster than using normal memory, in certain cases.
      There is a limited amount of such memory, and this will return [None] if none is available at present. *)

  val alloc_fixed_or_wait : unit -> Uring.Region.chunk
  (** Like {!alloc_fixed}, but if there are no chunks available then it waits until one is. *)

  val free_fixed : Uring.Region.chunk -> unit

  val with_chunk : fallback:(unit -> 'a) -> (Uring.Region.chunk -> 'a) -> 'a
  (** [with_chunk ~fallback fn] runs [fn chunk] with a freshly allocated chunk and then frees it.

      If no chunks are available, it runs [fallback ()] instead. *)

  (** {1 File manipulation functions} *)

  val openat2 :
    sw:Switch.t ->
    ?seekable:bool ->
    access:[`R|`W|`RW] ->
    flags:Uring.Open_flags.t ->
    perm:Unix.file_perm ->
    resolve:Uring.Resolve.t ->
    ?dir:fd -> string -> fd
  (** [openat2 ~sw ~flags ~perm ~resolve ~dir path] opens [dir/path].

      See {!Uring.openat2} for details. *)

  val read_upto : ?file_offset:Optint.Int63.t -> fd -> Uring.Region.chunk -> int -> int
  (** [read_upto fd chunk len] reads at most [len] bytes from [fd],
      returning as soon as some data is available.

      @param file_offset Read from the given position in [fd] (default: 0).
      @raise End_of_file Raised if all data has already been read. *)

  val read_exactly : ?file_offset:Optint.Int63.t -> fd -> Uring.Region.chunk -> int -> unit
  (** [read_exactly fd chunk len] reads exactly [len] bytes from [fd],
      performing multiple read operations if necessary.

      @param file_offset Read from the given position in [fd] (default: 0).
      @raise End_of_file Raised if the stream ends before [len] bytes have been read. *)

  val readv : ?file_offset:Optint.Int63.t -> fd -> Cstruct.t list -> int
  (** [readv] is like {!read_upto} but can read into any cstruct(s),
      not just chunks of the pre-shared buffer.

      If multiple buffers are given, they are filled in order. *)

  val write : ?file_offset:Optint.Int63.t -> fd -> Uring.Region.chunk -> int -> unit
  (** [write fd buf len] writes exactly [len] bytes from [buf] to [fd].

      It blocks until the OS confirms the write is done,
      and resubmits automatically if the OS doesn't write all of it at once. *)

  val writev : ?file_offset:Optint.Int63.t -> fd -> Cstruct.t list -> unit
  (** [writev] is like {!write} but can write from any cstruct(s),
      not just chunks of the pre-shared buffer.

      If multiple buffers are given, they are sent in order.
      It will make multiple OS calls if the OS doesn't write all of it at once. *)

  val writev_single : ?file_offset:Optint.Int63.t -> fd -> Cstruct.t list -> int
  (** [writev_single] is like [writev] but only performs a single write operation.
      It returns the number of bytes written, which may be smaller than the requested amount. *)

  val splice : fd -> dst:fd -> len:int -> int
  (** [splice src ~dst ~len] attempts to copy up to [len] bytes of data from [src] to [dst].

      @return The number of bytes copied.
      @raise End_of_file [src] is at the end of the file.
      @raise Unix.Unix_error(EINVAL, "splice", _) if splice is not supported for these FDs. *)

  val connect : fd -> Unix.sockaddr -> unit
  (** [connect fd addr] attempts to connect socket [fd] to [addr]. *)

  val await_readable : fd -> unit
  (** [await_readable fd] blocks until [fd] is readable (or has an error). *)

  val await_writable : fd -> unit
  (** [await_writable fd] blocks until [fd] is writable (or has an error). *)

  val statx : ?fd:fd -> mask:Uring.Statx.Mask.t -> string -> Uring.Statx.t -> Uring.Statx.Flags.t -> unit
  (** [statx t ?fd ~mask path buf flags] stats [path], which is resolved relative to [fd]
      (or the current directory if [fd] is not given).

      The results are written to [buf]. *)

  val read_dir : fd -> string list
  (** [read_dir dir] reads all directory entries from [dir].
      The entries are not returned in any particular order
      (not even necessarily the order in which Linux returns them). *)

  (** {1 Sockets} *)

  val accept : sw:Switch.t -> fd -> (fd * Unix.sockaddr)
  (** [accept ~sw t] blocks until a new connection is received on listening socket [t].

      It returns the new connection and the address of the connecting peer.
      The new connection has the close-on-exec flag set automatically.
      The new connection is attached to [sw] and will be closed when that finishes, if
      not already closed manually by then. *)

  val shutdown : fd -> Unix.shutdown_command -> unit
  (** Like {!Unix.shutdown}. *)

  val send_msg : fd -> ?fds:fd list -> ?dst:Unix.sockaddr -> Cstruct.t list -> int
  (** [send_msg socket bufs] is like [writev socket bufs], but also allows setting the destination address
      (for unconnected sockets) and attaching FDs (for Unix-domain sockets). *)

  val recv_msg : fd -> Cstruct.t list -> Uring.Sockaddr.t * int
  (** [recv_msg socket bufs] is like [readv socket bufs] but also returns the address of the sender. *)

  val recv_msg_with_fds : sw:Switch.t -> max_fds:int -> fd -> Cstruct.t list -> Uring.Sockaddr.t * int * fd list
  (** [recv_msg_with_fds] is like [recv_msg] but also allows receiving up to [max_fds] file descriptors
      (sent using SCM_RIGHTS over a Unix domain socket). *)

  (** {1 Randomness} *)

  val getrandom : Cstruct.t -> unit
  (**[getrandom buf] fills [buf] with random bytes.

     It uses Linux's [getrandom] call, which is like reading from /dev/urandom
     except that it will block (the whole domain) if used at early boot
     when the random system hasn't been initialised yet. *)

  (** {1 DNS functions} *)

  val getaddrinfo : service:string -> string -> Eio.Net.Sockaddr.t list
  (** [getaddrinfo host] returns a list of IP addresses for [host]. [host] is either a domain name or
      an ipaddress. *)

  (** {1 Processes} *)

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

end
