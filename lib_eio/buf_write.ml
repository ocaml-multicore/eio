(*----------------------------------------------------------------------------
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


type bigstring = Bigstringaf.t

exception Dequeue_empty

module Deque(T:sig type t val sentinel : t end) : sig
  type elem = T.t

  type t

  val create : int -> t

  val is_empty : t -> bool

  val enqueue : elem -> t -> unit
  val dequeue_exn : t -> elem
  val enqueue_front : elem -> t -> unit

  val map_to_list : t -> f:(elem -> 'b) -> 'b list
end = struct
  type elem = T.t

  type t =
    { mutable elements : elem array
    ; mutable front    : int
    ; mutable back     : int }

  let sentinel = T.sentinel

  let create size =
    { elements = Array.make size sentinel; front = 0; back = 0 }

  let is_empty t =
    t.front = t.back

  let ensure_space t =
    if t.back = Array.length t.elements - 1 then begin
      let len = t.back - t.front in
      if t.front > 0 then begin
        (* Shift everything to the front of the array and then clear out
         * dangling pointers to elements from their previous locations. *)
        Array.blit t.elements t.front t.elements 0 len;
        Array.fill t.elements len t.front sentinel
      end else begin
        let old  = t.elements in
        let new_ = Array.(make (2 * length old) sentinel) in
        Array.blit old t.front new_ 0 len;
        t.elements <- new_
      end;
      t.front <- 0;
      t.back <- len
    end

  let enqueue e t =
    ensure_space t;
    t.elements.(t.back) <- e;
    t.back <- t.back + 1

  let dequeue_exn t =
    if is_empty t then
      raise Dequeue_empty
    else
      let result = Array.unsafe_get t.elements t.front in
      Array.unsafe_set t.elements t.front sentinel;
      t.front <- t.front + 1;
      result

  let enqueue_front e t =
    (* This is in general not true for Deque data structures, but the usage
     * below ensures that there is always space to push an element back on the
     * front. An [enqueue_front] is always preceded by a [dequeue], with no
     * intervening operations. *)
    assert (t.front > 0);
    t.front <- t.front - 1;
    t.elements.(t.front) <- e

  let map_to_list t ~f =
    let result = ref [] in
    for i = t.back - 1 downto t.front do
      result := f t.elements.(i) :: !result
    done;
    !result
end

module Buffers = Deque(struct
  type t = Cstruct.t
  let sentinel =
    let deadbeef = "\222\173\190\239" in
    let len      = String.length deadbeef in
    let buffer   = Bigstringaf.create len in
    String.iteri (Bigstringaf.unsafe_set buffer) deadbeef;
    Cstruct.of_bigarray buffer ~len
end)
module Flushes = Deque(struct
  type t = int * (unit -> unit)
  let sentinel = 0, fun () -> ()
end)

type t =
  { mutable buffer         : bigstring
  ; mutable scheduled_pos  : int
  ; mutable write_pos      : int
  ; scheduled              : Buffers.t
  ; flushed                : Flushes.t
  ; mutable bytes_received : int
  ; mutable bytes_written  : int
  ; mutable closed         : bool
  ; mutable yield          : bool
  }

type operation = [
  | `Writev of Cstruct.t list
  | `Yield
  | `Close
  ]

let of_bigstring buffer =
  { buffer
  ; write_pos       = 0
  ; scheduled_pos   = 0
  ; scheduled       = Buffers.create 4
  ; flushed         = Flushes.create 1
  ; bytes_received  = 0
  ; bytes_written   = 0
  ; closed          = false
  ; yield           = false }

let create size =
  of_bigstring (Bigstringaf.create size)

let writable_exn t =
  if t.closed then
    failwith "cannot write to closed writer"

let schedule_iovec t ?(off=0) ~len buffer =
  t.bytes_received <- t.bytes_received + len;
  Buffers.enqueue (Cstruct.of_bigarray buffer ~off ~len) t.scheduled

let flush_buffer t =
  let len = t.write_pos - t.scheduled_pos in
  if len > 0 then begin
    let off = t.scheduled_pos in
    schedule_iovec t ~off ~len t.buffer;
    t.scheduled_pos <- t.write_pos
  end

let flush t f =
  t.yield <- false;
  flush_buffer t;
  if Buffers.is_empty t.scheduled then f ()
  else Flushes.enqueue (t.bytes_received, f) t.flushed

let free_bytes_in_buffer t =
  let buf_len = Bigstringaf.length t.buffer in
  buf_len - t.write_pos

let schedule_bigstring t ?(off=0) ?len a =
  writable_exn t;
  flush_buffer t;
  let len =
    match len with
    | None     -> Bigstringaf.length a - off
    | Some len -> len
  in
  if len > 0 then schedule_iovec t ~off ~len a

let ensure_space t len =
  if free_bytes_in_buffer t < len then begin
    flush_buffer t;
    t.buffer <- Bigstringaf.create (max (Bigstringaf.length t.buffer) len);
    t.write_pos <- 0;
    t.scheduled_pos <- 0
  end

let write_gen t ~length ~blit ?(off=0) ?len a =
  writable_exn t;
  let len =
    match len with
    | None     -> length a - off
    | Some len -> len
  in
  ensure_space t len;
  blit a ~src_off:off t.buffer ~dst_off:t.write_pos ~len;
  t.write_pos <- t.write_pos + len

let write_string =
  let length   = String.length in
  let blit     = Bigstringaf.unsafe_blit_from_string in
  fun t ?off ?len a -> write_gen t ~length ~blit ?off ?len a

let write_bytes =
  let length = Bytes.length in
  let blit   = Bigstringaf.unsafe_blit_from_bytes in
  fun t ?off ?len a -> write_gen t ~length ~blit ?off ?len a

let write_bigstring =
  let length = Bigstringaf.length in
  let blit   = Bigstringaf.unsafe_blit in
  fun t ?off ?len a -> write_gen t ~length ~blit ?off ?len a

let write_char t c =
  writable_exn t;
  ensure_space t 1;
  Bigstringaf.unsafe_set t.buffer t.write_pos c;
  t.write_pos <- t.write_pos + 1

let write_uint8 t b =
  writable_exn t;
  ensure_space t 1;
  Bigstringaf.unsafe_set t.buffer t.write_pos (Char.unsafe_chr b);
  t.write_pos <- t.write_pos + 1

module BE = struct
  let write_uint16 t i =
    writable_exn t;
    ensure_space t 2;
    Bigstringaf.unsafe_set_int16_be t.buffer t.write_pos i;
    t.write_pos <- t.write_pos + 2

  let write_uint32 t i =
    writable_exn t;
    ensure_space t 4;
    Bigstringaf.unsafe_set_int32_be t.buffer t.write_pos i;
    t.write_pos <- t.write_pos + 4

  let write_uint48 t i =
    writable_exn t;
    ensure_space t 6;
    Bigstringaf.unsafe_set_int32_be t.buffer t.write_pos
      Int64.(to_int32 (shift_right_logical i 4));
    Bigstringaf.unsafe_set_int16_be t.buffer (t.write_pos + 2)
      Int64.(to_int i);
    t.write_pos <- t.write_pos + 6

  let write_uint64 t i =
    writable_exn t;
    ensure_space t 8;
    Bigstringaf.unsafe_set_int64_be t.buffer t.write_pos i;
    t.write_pos <- t.write_pos + 8

  let write_float t f =
    writable_exn t;
    ensure_space t 4;
    Bigstringaf.unsafe_set_int32_be t.buffer t.write_pos (Int32.bits_of_float f);
    t.write_pos <- t.write_pos + 4

  let write_double t d =
    writable_exn t;
    ensure_space t 8;
    Bigstringaf.unsafe_set_int64_be t.buffer t.write_pos (Int64.bits_of_float d);
    t.write_pos <- t.write_pos + 8
end

module LE = struct
  let write_uint16 t i =
    writable_exn t;
    ensure_space t 2;
    Bigstringaf.unsafe_set_int16_le t.buffer t.write_pos i;
    t.write_pos <- t.write_pos + 2

  let write_uint32 t i =
    writable_exn t;
    ensure_space t 4;
    Bigstringaf.unsafe_set_int32_le t.buffer t.write_pos i;
    t.write_pos <- t.write_pos + 4

  let write_uint48 t i =
    writable_exn t;
    ensure_space t 6;
    Bigstringaf.unsafe_set_int16_le t.buffer t.write_pos
      Int64.(to_int i);
    Bigstringaf.unsafe_set_int32_le t.buffer (t.write_pos + 2)
      Int64.(to_int32 (shift_right_logical i 2));
    t.write_pos <- t.write_pos + 6

  let write_uint64 t i =
    writable_exn t;
    ensure_space t 8;
    Bigstringaf.unsafe_set_int64_le t.buffer t.write_pos i;
    t.write_pos <- t.write_pos + 8

  let write_float t f =
    writable_exn t;
    ensure_space t 4;
    Bigstringaf.unsafe_set_int32_le t.buffer t.write_pos (Int32.bits_of_float f);
    t.write_pos <- t.write_pos + 4

  let write_double t d =
    writable_exn t;
    ensure_space t 8;
    Bigstringaf.unsafe_set_int64_le t.buffer t.write_pos (Int64.bits_of_float d);
    t.write_pos <- t.write_pos + 8
end

let close t =
  t.closed <- true;
  flush_buffer t

let is_closed t =
  t.closed

let pending_bytes t =
  (t.write_pos - t.scheduled_pos) + (t.bytes_received - t.bytes_written)

let has_pending_output t =
  pending_bytes t <> 0

let yield t =
  t.yield <- true

let rec shift_buffers t written =
  try
    let { Cstruct.len; _ } as iovec = Buffers.dequeue_exn t.scheduled in
    if len <= written then begin
      shift_buffers t (written - len)
    end else
      Buffers.enqueue_front (Cstruct.shift iovec written) t.scheduled
  with Dequeue_empty ->
    assert (written = 0);
    if t.scheduled_pos = t.write_pos then begin
      t.scheduled_pos <- 0;
      t.write_pos <- 0
    end

let rec shift_flushes t =
  try
    let (threshold, f) as flush = Flushes.dequeue_exn t.flushed in
    (* Edited notes from @dinosaure:
     *
     * The quantities [t.bytes_written] and [threshold] are always going to be
     * positive integers. Therefore, we can treat them as unsinged integers for
     * the purposes of comparision. Doing so allows us to handle overflows in
     * either quantity as long as they're both within one overflow of each other.
     * We can accomplish this by subracting [min_int] from both quantities before
     * comparision. This shift a quantity that has not overflowed into the
     * negative integer range while shifting a quantity that has overflow into
     * the positive integer range.
     *
     * This effectively restablishes the relative difference when an overflow
     * has occurred, and otherwise just compares numbers that haven't
     * overflowed as similarly, just shifted down a bit.
     *)
    if t.bytes_written - min_int >= threshold - min_int
    then begin f (); shift_flushes t end
    else Flushes.enqueue_front flush t.flushed
  with Dequeue_empty ->
    ()

let shift t written =
  shift_buffers t written;
  t.bytes_written <- t.bytes_written + written;
  shift_flushes t

let operation t =
  if t.closed then begin
    t.yield <- false
  end;
  flush_buffer t;
  let nothing_to_do = not (has_pending_output t) in
  if t.closed && nothing_to_do then
    `Close
  else if t.yield || nothing_to_do then begin
    t.yield <- false;
    `Yield
  end else begin
    let iovecs = Buffers.map_to_list t.scheduled ~f:(fun x -> x) in
    `Writev iovecs
  end

let rec serialize t writev =
  match operation t with
  | `Writev iovecs ->
    begin match writev iovecs with
    | `Ok   n -> shift t n; if not (Buffers.is_empty t.scheduled) then yield t
    | `Closed -> close t
    end;
    serialize t writev
  | (`Close|`Yield) as next -> next

let serialize_to_string t =
  close t;
  match operation t with
  | `Writev iovecs ->
    let len = Cstruct.lenv iovecs in
    let bytes = Bytes.create len in
    let pos = ref 0 in
    List.iter (function
      | { Cstruct.buffer; off; len } ->
        Bigstringaf.unsafe_blit_to_bytes buffer ~src_off:off bytes ~dst_off:!pos ~len;
        pos := !pos + len)
    iovecs;
    shift t len;
    assert (operation t = `Close);
    Bytes.unsafe_to_string bytes
  | `Close -> ""
  | `Yield -> assert false

let serialize_to_bigstring t =
  close t;
  match operation t with
  | `Writev iovecs ->
    let len = Cstruct.lenv iovecs in
    let bs = Bigstringaf.create len in
    let pos = ref 0 in
    List.iter (function
      | { Cstruct.buffer; off; len } ->
        Bigstringaf.unsafe_blit buffer ~src_off:off bs ~dst_off:!pos ~len;
        pos := !pos + len)
    iovecs;
    shift t len;
    assert (operation t = `Close);
    bs
  | `Close -> Bigstringaf.create 0
  | `Yield -> assert false

let drain =
  let rec loop t acc =
    match operation t with
    | `Writev iovecs ->
      let len = Cstruct.lenv iovecs in
      shift t len;
      loop t (len + acc)
    | `Close         -> acc
    | `Yield         -> loop t acc
  in
  fun t -> loop t 0
