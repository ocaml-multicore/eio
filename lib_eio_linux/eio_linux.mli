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

module Low_level = Low_level
