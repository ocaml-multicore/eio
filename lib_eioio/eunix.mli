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

type t

(** {1 Fibre functions} *)

val fork : (unit -> 'a) -> 'a Promise.t
(** [fork fn] starts running [fn ()] and returns a promise for its result. *)

val yield : unit -> unit

val sleep : float -> unit

(** {1 Memory allocation functions} *)

val alloc : unit -> Uring.Region.chunk

val free : Uring.Region.chunk -> unit

(** {1 File manipulation functions} *)

val read_upto : ?file_offset:int -> Unix.file_descr -> Uring.Region.chunk -> int -> int
(** [read_upto fd chunk len] reads at most [len] bytes from [fd],
    returning as soon as some data is available.
    @param file_offset Read from the given position in [fd] (default: 0).
    @raise End_of_file Raised if all data has already been read. *)

val read_exactly : ?file_offset:int -> Unix.file_descr -> Uring.Region.chunk -> int -> unit
(** [read_exactly fd chunk len] reads exactly [len] bytes from [fd],
    performing multiple read operations if necessary.
    @param file_offset Read from the given position in [fd] (default: 0).
    @raise End_of_file Raised if the stream ends before [len] bytes have been read. *)

val write : ?file_offset:int -> Unix.file_descr -> Uring.Region.chunk -> int -> unit

val await_readable : Unix.file_descr -> unit
(** [await_readable fd] blocks until [fd] is readable (or has an error). *)

val await_writable : Unix.file_descr -> unit
(** [await_writable fd] blocks until [fd] is writable (or has an error). *)

(** {1 Main Loop} *)

val run : ?queue_depth:int -> ?block_size:int -> (unit -> unit) -> unit
(** FIXME queue_depth and block_size should be in a handler and not the mainloop *)
