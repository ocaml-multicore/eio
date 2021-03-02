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
val fork : (unit -> unit) -> unit

val yield : unit -> unit

val sleep : float -> unit

(** {1 Memory allocation functions} *)

val alloc : unit -> Uring.Region.chunk

val free : Uring.Region.chunk -> unit

(** {1 File manipulation functions} *)
val read : ?file_offset:int -> Unix.file_descr -> Uring.Region.chunk -> int -> unit

val write : ?file_offset:int -> Unix.file_descr -> Uring.Region.chunk -> int -> unit

(** {1 Main Loop} *)

val run : ?queue_depth:int -> ?block_size:int -> (unit -> unit) -> unit
(** FIXME queue_depth and block_size should be in a handler and not the mainloop *)
