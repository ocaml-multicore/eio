(* This module is based on code from Faraday (0.7.2), which had the following
   license:

  ----------------------------------------------------------------------------
    Copyright (c) 2016 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

(** Serialization primitives built for speed and memory-efficiency.


    Buf_write is designed for writing fast and memory-efficient serializers.
    It is based on the Faraday library, but adapted for Eio.
    Its core type and related operation gives the user fine-grained control
    over copying and allocation behavior while serializing user-defined types,
    and presents the output in a form that makes it possible to use vectorized
    write operations, such as the [writev][] system call, or any other platform
    or application-specific output APIs.

    A Buf_write serializer manages an internal buffer and a queue of output
    buffers. The output bufferes may be a sub range of the serializer's
    internal buffer or one that is user-provided. Buffered writes such as
    {!string}, {!char}, {!cstruct}, etc., copy the source bytes into the
    serializer's internal buffer. Unbuffered writes are done with
    {!schedule_cstruct}, which performs no copying. Instead, it enqueues the
    source bytes into the serializer's write queue directly.

    Example:

    {[
      module Write = Eio.Buf_write

      let () =
          Eio_mock.Backend.run @@ fun () ->
          let stdout = Eio_mock.Flow.make "stdout" in
          Write.with_flow stdout (fun w ->
              Write.string w "foo";
              Write.string w "bar";
              Eio.Fiber.yield ();
              Write.string w "baz";
          )
    ]}

    This combines the first two writes, giving:

    {[
      +stdout: wrote "foobar"
      +stdout: wrote "baz"
    ]}
 *)

type t
(** The type of a serializer. *)

(** {2 Running} *)

val with_flow : ?initial_size:int -> #Flow.sink -> (t -> 'a) -> 'a
(** [with_flow flow fn] runs [fn writer], where [writer] is a buffer that flushes to [flow].

    Concurrently with [fn], it also runs a fiber that copies from [writer] to [flow].
    If this fiber runs out of data to copy then it will suspend itself.
    Writing to [writer] will automatically schedule it to be resumed.
    This means that pending data is flushed automatically before the process sleeps.

    When [fn] returns, [writer] is automatically closed and any remaining data is flushed
    before [with_flow] itself returns.

    @param initial_size The initial size of the buffer used to collect writes.
                        New buffers will be allocated as needed, with the same size.
                        If the buffer is too small to contain a write, the size is increased. *)


(** {2 Buffered Writes}

    A serializer manages an internal buffer for coalescing small writes. The
    size of this buffer is determined when the serializer is created. If the
    buffer does not contain sufficient space to service a caller's buffered
    write, the serializer will allocate a new buffer of the sufficient size and
    use it for the current and subsequent writes. The old buffer will be
    garbage collected once all of its contents have been {!flush}ed. *)

val string : t -> ?off:int -> ?len:int -> string -> unit
(** [string t ?off ?len str] copies [str] into the serializer's
    internal buffer. *)

val bytes : t -> ?off:int -> ?len:int -> Bytes.t -> unit
(** [bytes t ?off ?len bytes] copies [bytes] into the serializer's
    internal buffer. It is safe to modify [bytes] after this call returns. *)

val cstruct : t -> Cstruct.t -> unit
(** [cstruct t cs] copies [cs] into the serializer's internal buffer.
    It is safe to modify [cs] after this call returns.
    For large cstructs, it may be more efficient to use {!schedule_cstruct}. *)

val write_gen
  :  t
  -> blit:('a -> src_off:int -> Cstruct.buffer -> dst_off:int -> len:int -> unit)
  -> off:int
  -> len:int
  -> 'a -> unit
(** [write_gen t ~blit ~off ~len x] copies [x] into the serializer's
    internal buffer using the provided [blit] operation.
    See {!Bigstring.blit} for documentation of the arguments. *)

val char : t -> char -> unit
(** [char t c] copies [c] into the serializer's internal buffer. *)

val uint8 : t -> int -> unit
(** [uint8 t n] copies the lower 8 bits of [n] into the serializer's
    internal buffer. *)


(** Big endian serializers *)
module BE : sig
  val uint16 : t -> int -> unit
  (** [uint16 t n] copies the lower 16 bits of [n] into the serializer's
      internal buffer in big-endian byte order. *)

  val uint32 : t -> int32 -> unit
  (** [uint32 t n] copies [n] into the serializer's internal buffer in
      big-endian byte order. *)

  val uint48 : t -> int64 -> unit
  (** [uint48 t n] copies the lower 48 bits of [n] into the serializer's
      internal buffer in big-endian byte order. *)

  val uint64 : t -> int64 -> unit
  (** [uint64 t n] copies [n] into the serializer's internal buffer in
      big-endian byte order. *)

  val float : t -> float -> unit
  (** [float t n] copies the lower 32 bits of [n] into the serializer's
      internal buffer in big-endian byte order. *)

  val double : t -> float -> unit
  (** [double t n] copies [n] into the serializer's internal buffer in
      big-endian byte order. *)
end


(** Little endian serializers *)
module LE : sig
  val uint16 : t -> int -> unit
  (** [uint16 t n] copies the lower 16 bits of [n] into the
      serializer's internal buffer in little-endian byte order. *)

  val uint32 : t -> int32 -> unit
  (** [uint32 t n] copies [n] into the serializer's internal buffer in
      little-endian byte order. *)

  val uint48 : t -> int64 -> unit
  (** [uint48 t n] copies the lower 48 bits of [n] into the serializer's
      internal buffer in little-endian byte order. *)

  val uint64 : t -> int64 -> unit
  (** [uint64 t n] copies [n] into the serializer's internal buffer in
      little-endian byte order. *)

  val float : t -> float -> unit
  (** [float t n] copies the lower 32 bits of [n] into the serializer's
      internal buffer in little-endian byte order. *)

  val double : t -> float -> unit
  (** [double t n] copies [n] into the serializer's internal buffer in
      little-endian byte order. *)
end


(** {2 Unbuffered Writes}

    Unbuffered writes do not involve copying bytes to the serializer's internal
    buffer. *)

val schedule_cstruct : t -> Cstruct.t -> unit
(** [schedule_cstruct t cs] schedules [cs] to be written.
    [cs] is not copied in this process,
    so [cs] should only be modified after [t] has been {!flush}ed. *)


(** {2 Querying A Serializer's State} *)

val free_bytes_in_buffer : t -> int
(** [free_bytes_in_buffer t] returns the free space, in bytes, of the
    serializer's write buffer. If a write call has a length that exceeds
    this value, the serializer will allocate a new buffer that will replace the
    serializer's internal buffer for that and subsequent calls. *)

val has_pending_output : t -> bool
(** [has_pending_output t] is [true] if [t]'s output queue is non-empty. It may
    be the case that [t]'s queued output is being serviced by some other thread
    of control, but has not yet completed. *)

val pending_bytes : t -> int
(** [pending_bytes t] is the size of the next write, in bytes, that [t] will
    surface to the caller via {!await_batch}. *)


(** {2 Control Operations} *)

val pause : t -> unit
(** [pause t] causes [t] to stop surfacing writes to the user.
    This gives the serializer an opportunity to collect additional writes
    before sending them to the underlying device, which will increase the write
    batch size.

    As one example, code may want to call this function if it's about to
    release the OCaml lock and perform a blocking system call, but would like
    to batch output across that system call.

    Call {!unpause} to resume writing later.
    Note that calling {!flush} or {!close} will automatically call {!unpause} too. *)

val unpause : t -> unit
(** [unpause t] resumes writing data after a previous call to {!pause}. *)

val flush : t -> unit
(** [flush t] waits until all prior writes have been successfully completed.
    If [t] has no pending writes, [flush] returns immediately.
    If [t] is paused then it is unpaused first. *)

val close : t -> unit
(** [close t] closes [t]. All subsequent write calls will raise, and any
    subsequent {!pause} calls will be ignored. If the serializer has
    any pending writes, user code will have an opportunity to service them
    before receiving [End_of_file]. Flush callbacks will continue to
    be invoked while output is {!shift}ed out of [t] as needed. *)

val is_closed : t -> bool
(** [is_closed t] is [true] if [close] has been called on [t] and [false]
    otherwise. A closed [t] may still have pending output. *)


(** {2 Low-level API}

    Low-level operations for running a serializer. *)

val create : sw:Switch.t -> int -> t
(** [create ~sw len] creates a serializer with a fixed-length internal buffer of
    length [len]. See the Buffered writes section for details about what happens
    when [len] is not large enough to support a write.
    When [sw] is finished, any pending flush operations immediately fail. *)

val of_buffer : Cstruct.buffer -> t
(** [of_buffer buf] creates a serializer, using [buf] as its internal
    buffer. The serializer takes ownership of [buf] until the serializer has
    been closed and flushed of all output. *)

val await_batch : t -> Cstruct.t list
(** [await_batch t] returns a list of buffers that should be written.
    If no data is currently available, it waits until some is.
    After performing a write, call {!shift} with the number of bytes written.
    You must accurately report the number of bytes written. Failure to do so
    will result in the same bytes being surfaced multiple times.
    @raises End_of_file [t] is closed and there is nothing left to write. *)

val shift : t -> int -> unit
(** [shift t n] removes the first [n] bytes in [t]'s write queue. Any flush
    operations called within this span of the write queue will be scheduled
    to resume. *)


(** {2 Convenience Functions}

    These functions are included for testing, debugging, and general
    development. They are not the suggested way of driving a serializer in a
    production setting. *)

val serialize : t -> (Cstruct.t list -> (int, [`Closed]) result) -> (unit, [> `Closed]) result
(** [serialize t writev] calls [writev bufs] each time [t] is ready to write.
    In the event that [writev] indicates a partial write, {!serialize} will
    call {!Fiber.yield} before continuing. *)

val serialize_to_string : t -> string
(** [serialize_to_string t] runs [t], collecting the output into a string and
    returning it. [serializie_to_string t] immediately closes [t]. *)

val serialize_to_cstruct : t -> Cstruct.t
(** [serialize_to_cstruct t] runs [t], collecting the output into a cstruct
    and returning it. [serialize_to_cstruct t] immediately closes [t]. *)

val drain : t -> int
(** [drain t] removes all pending writes from [t], returning the number of
    bytes that were enqueued to be written and freeing any scheduled
    buffers in the process. Note that this does not close [t] itself,
    and does not return until [t] has been closed. *)
