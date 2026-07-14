(*
 * Copyright (C) 2020-2021 Anil Madhavapeddy
 * Copyright (C) 2023 Thomas Leonard
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

module Mono_clock = struct
  type t = unit
  type time = Mtime.t

  let now () = Mtime_clock.now ()
  let sleep_until () time = Low_level.sleep_until time
end

let mono_clock : Mtime.t Eio.Time.clock_ty r =
  let handler = Eio.Time.Pi.clock (module Mono_clock) in
  Eio.Resource.T ((), handler)

module Clock = struct
  type t = unit
  type time = float

  let now () = Unix.gettimeofday ()

  let sleep_until () time =
    (* todo: use the realtime clock directly instead of converting to monotonic time.
       That is needed to handle adjustments to the system clock correctly. *)
    let d = time -. Unix.gettimeofday () in
    Eio.Time.Mono.sleep mono_clock d
end

let clock : float Eio.Time.clock_ty r =
  let handler = Eio.Time.Pi.clock (module Clock) in
  Eio.Resource.T ((), handler)
