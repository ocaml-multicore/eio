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


type bigstring = Bigstringaf.t

exception Dequeue_empty

module Deque(T:sig type t val sentinel : t end) : sig
  type elem = T.t

  type t

  val create : int -> t
  (* [t = create n] creates a new deque with initial capacity [n].

     [to_list t = []] *)

  val is_empty : t -> bool
  (* [is_empty t = (to_list t = []) *)

  val enqueue : elem -> t -> unit
  (* [enqueue elem t]

     [to_list t'] = to_list t @ [elem] *)

  val dequeue_exn : t -> elem
  (* [dequeue_exn t = List.hd (to_list t)]

     [to_list t' = List.tl (to_list t)] *)

  val enqueue_front : elem -> t -> unit
  (* [enqueue_front elem t]

     to_list t' = elem :: to_list t *)

  val to_list : t -> elem list
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

  let to_list t =
    let result = ref [] in
    for i = t.back - 1 downto t.front do
      result := t.elements.(i) :: !result
    done;
    !result
end

module Buffers = Deque(struct
  type t = Cstruct.t
  let sentinel =
    let deadbeef = "\222\173\190\239" in
    Cstruct.of_string deadbeef
end)

module Flushes = Deque(struct
    type t = int * ((unit, exn) result Promise.u)
    let sentinel =
      let _, r = Promise.create () in
      Promise.resolve_ok r ();
      0, r
  end)

type state =
  | Active
  | Paused
  | Closed

type t =
  { mutable buffer         : bigstring
  ; mutable scheduled_pos  : int        (* How much of [buffer] is in [scheduled] *)
  ; mutable write_pos      : int        (* How much of [buffer] has been written to *)
  ; scheduled              : Buffers.t
  ; flushed                : Flushes.t
  ; mutable bytes_received : int        (* Total scheduled bytes. Wraps. *)
  ; mutable bytes_written  : int        (* Total written bytes. Wraps. *)
  ; mutable state          : state
  ; mutable wake_writer    : unit -> unit
  ; id                     : Ctf.id
  }
(* Invariant: [write_pos >= scheduled_pos] *)

exception Flush_aborted

let writable_exn t =
  match t.state with
  | Active | Paused -> ()
  | Closed ->
    failwith "cannot write to closed writer"

let wake_writer t =
  match t.state with
  | Paused -> ()
  | Active | Closed ->
    let wake = t.wake_writer in
    if wake != ignore then (
      t.wake_writer <- ignore;
      wake ()
    )

(* Schedule [cs] now, without any checks. Users use {!schedule_cstruct} instead. *)
let schedule_iovec t cs =
  t.bytes_received <- t.bytes_received + Cstruct.length cs;
  Buffers.enqueue cs t.scheduled

(* Schedule all pending data in [buffer]. *)
let flush_buffer t =
  let len = t.write_pos - t.scheduled_pos in
  if len > 0 then begin
    let off = t.scheduled_pos in
    schedule_iovec t (Cstruct.of_bigarray ~off ~len t.buffer);
    t.scheduled_pos <- t.write_pos
  end

let free_bytes_in_buffer t =
  let buf_len = Bigstringaf.length t.buffer in
  buf_len - t.write_pos

let schedule_cstruct t cs =
  writable_exn t;
  flush_buffer t;
  if Cstruct.length cs > 0 then (
    schedule_iovec t cs;
    wake_writer t;
  )

let ensure_space t len =
  if free_bytes_in_buffer t < len then begin
    flush_buffer t;
    t.buffer <- Bigstringaf.create (max (Bigstringaf.length t.buffer) len);
    t.write_pos <- 0;
    t.scheduled_pos <- 0
  end

let advance_pos t n =
  t.write_pos <- t.write_pos + n;
  wake_writer t

let write_gen t ~blit ~off ~len a =
  writable_exn t;
  ensure_space t len;
  blit a ~src_off:off t.buffer ~dst_off:t.write_pos ~len;
  advance_pos t len

let string =
  let blit = Bigstringaf.blit_from_string in
  fun t ?(off=0) ?len a ->
    let len =
      match len with
      | None     -> String.length a - off
      | Some len -> len
    in
    write_gen t ~blit ~off ~len a

let bytes =
  let blit = Bigstringaf.blit_from_bytes in
  fun t ?(off=0) ?len a ->
    let len =
      match len with
      | None     -> Bytes.length a - off
      | Some len -> len
    in
    write_gen t ~blit ~off ~len a

let cstruct t { Cstruct.buffer; off; len } =
  write_gen t ~off ~len buffer
    ~blit:Bigstringaf.unsafe_blit

let char t c =
  writable_exn t;
  ensure_space t 1;
  Bigstringaf.unsafe_set t.buffer t.write_pos c;
  advance_pos t 1

let uint8 t b =
  writable_exn t;
  ensure_space t 1;
  Bigstringaf.unsafe_set t.buffer t.write_pos (Char.unsafe_chr b);
  advance_pos t 1

module BE = struct
  let uint16 t i =
    writable_exn t;
    ensure_space t 2;
    Bigstringaf.unsafe_set_int16_be t.buffer t.write_pos i;
    advance_pos t 2

  let uint32 t i =
    writable_exn t;
    ensure_space t 4;
    Bigstringaf.unsafe_set_int32_be t.buffer t.write_pos i;
    advance_pos t 4

  let uint48 t i =
    writable_exn t;
    ensure_space t 6;
    Bigstringaf.unsafe_set_int32_be t.buffer t.write_pos
      Int64.(to_int32 (shift_right_logical i 4));
    Bigstringaf.unsafe_set_int16_be t.buffer (t.write_pos + 2)
      Int64.(to_int i);
    advance_pos t 6

  let uint64 t i =
    writable_exn t;
    ensure_space t 8;
    Bigstringaf.unsafe_set_int64_be t.buffer t.write_pos i;
    advance_pos t 8

  let float t f =
    writable_exn t;
    ensure_space t 4;
    Bigstringaf.unsafe_set_int32_be t.buffer t.write_pos (Int32.bits_of_float f);
    advance_pos t 4

  let double t d =
    writable_exn t;
    ensure_space t 8;
    Bigstringaf.unsafe_set_int64_be t.buffer t.write_pos (Int64.bits_of_float d);
    advance_pos t 8
end

module LE = struct
  let uint16 t i =
    writable_exn t;
    ensure_space t 2;
    Bigstringaf.unsafe_set_int16_le t.buffer t.write_pos i;
    advance_pos t 2

  let uint32 t i =
    writable_exn t;
    ensure_space t 4;
    Bigstringaf.unsafe_set_int32_le t.buffer t.write_pos i;
    advance_pos t 4

  let uint48 t i =
    writable_exn t;
    ensure_space t 6;
    Bigstringaf.unsafe_set_int16_le t.buffer t.write_pos
      Int64.(to_int i);
    Bigstringaf.unsafe_set_int32_le t.buffer (t.write_pos + 2)
      Int64.(to_int32 (shift_right_logical i 2));
    advance_pos t 6

  let uint64 t i =
    writable_exn t;
    ensure_space t 8;
    Bigstringaf.unsafe_set_int64_le t.buffer t.write_pos i;
    advance_pos t 8

  let float t f =
    writable_exn t;
    ensure_space t 4;
    Bigstringaf.unsafe_set_int32_le t.buffer t.write_pos (Int32.bits_of_float f);
    advance_pos t 4

  let double t d =
    writable_exn t;
    ensure_space t 8;
    Bigstringaf.unsafe_set_int64_le t.buffer t.write_pos (Int64.bits_of_float d);
    advance_pos t 8
end

let close t =
  t.state <- Closed;
  flush_buffer t;
  wake_writer t

let is_closed t =
  match t.state with
  | Closed -> true
  | Active | Paused -> false

let abort t =
  close t;
  let rec aux () =
    match Flushes.dequeue_exn t.flushed with
    | exception Dequeue_empty -> ()
    | (_threshold, r) ->
      Promise.resolve_error r Flush_aborted;
      aux ()
  in
  aux ()

let of_buffer ?sw buffer =
  let t = { buffer
          ; write_pos       = 0
          ; scheduled_pos   = 0
          ; scheduled       = Buffers.create 4
          ; flushed         = Flushes.create 1
          ; bytes_received  = 0
          ; bytes_written   = 0
          ; state           = Active
          ; wake_writer     = ignore
          ; id              = Ctf.mint_id ()
          }
  in
  begin match sw with
    | Some sw -> Switch.on_release sw (fun () -> abort t)
    | None -> ()
  end;
  t

let create ?sw size =
  of_buffer ?sw (Bigstringaf.create size)

let pending_bytes t =
  (t.write_pos - t.scheduled_pos) + (t.bytes_received - t.bytes_written)

let has_pending_output t =
  pending_bytes t <> 0

let pause t =
  match t.state with
  | Active -> t.state <- Paused
  | Paused | Closed -> ()

let unpause t =
  match t.state with
  | Active | Closed -> ()
  | Paused ->
    t.state <- Active;
    if has_pending_output t then
      wake_writer t

let flush t =
  flush_buffer t;
  unpause t;
  if not (Buffers.is_empty t.scheduled) then (
    let p, r = Promise.create () in
    Flushes.enqueue (t.bytes_received, r) t.flushed;
    Promise.await_exn p
  )

let rec shift_buffers t written =
  match Buffers.dequeue_exn t.scheduled with
  | { Cstruct.len; _ } as iovec ->
    if len <= written then
      shift_buffers t (written - len)
    else
      Buffers.enqueue_front (Cstruct.shift iovec written) t.scheduled
  | exception Dequeue_empty ->
    assert (written = 0);
    if t.scheduled_pos = t.write_pos then begin
      t.scheduled_pos <- 0;
      t.write_pos <- 0
    end

(* Resolve any flushes that are now due. *)
let rec shift_flushes t =
  match Flushes.dequeue_exn t.flushed with
  | exception Dequeue_empty -> ()
  | (threshold, r) as flush ->
    (* Be careful: [bytes_written] and [threshold] both wrap, so subtract first. *)
    if t.bytes_written - threshold >= 0 then (
      (* We have written at least up to [threshold]
         (or we're more than [max_int] behind, which we assume won't happen). *)
      Promise.resolve_ok r ();
      shift_flushes t
    ) else (
      Flushes.enqueue_front flush t.flushed
    )

let shift t written =
  shift_buffers t written;
  t.bytes_written <- t.bytes_written + written;
  shift_flushes t

let rec await_batch t =
  flush_buffer t;
  match t.state, has_pending_output t with
  | Closed, false -> raise End_of_file
  | (Active | Closed), true -> Buffers.to_list t.scheduled
  | Paused, _ | Active, false ->
    Suspend.enter (fun ctx enqueue ->
        Fiber_context.set_cancel_fn ctx (fun ex ->
            t.wake_writer <- ignore;
            enqueue (Error ex)
          );
        t.wake_writer <- (fun () ->
            (* Our caller has already set [wake_writer <- ignore]. *)
            Fiber_context.clear_cancel_fn ctx;
            enqueue (Ok ())
          );
      );
    await_batch t

let read_into t buf =
  let iovecs = await_batch t in
  let n, _iovecs = Cstruct.fillv ~src:iovecs ~dst:buf in
  shift t n;
  n

let read_source_buffer t fn =
  let iovecs = await_batch t in
  shift t (fn iovecs)

let as_flow t =
  object
    inherit Flow.source
    method! read_methods = [Flow.Read_source_buffer (read_source_buffer t)]
    method read_into = read_into t
  end

let with_flow ?(initial_size=0x1000) flow fn =
  Switch.run @@ fun sw ->
  let t = create ~sw initial_size in
  Fiber.fork ~sw (fun () -> Flow.copy (as_flow t) flow);
  match fn t with
  | x ->
    close t;
    x
  | exception ex ->
    close t;
    (* Raising the exception will cancel the writer thread, so do a flush first.
       We don't want to flush if cancelled, but in that case the switch will
       end the writer thread itself (and [flush] will raise). *)
    flush t;
    raise ex

let rec serialize t writev =
  match await_batch t with
  | exception End_of_file -> Ok ()
  | iovecs ->
    match writev iovecs with
    | Error `Closed as e -> close t; e
    | Ok n ->
      shift t n;
      if not (Buffers.is_empty t.scheduled) then Fiber.yield ();
      serialize t writev

let serialize_to_string t =
  close t;
  match await_batch t with
  | exception End_of_file -> ""
  | iovecs ->
    let len = Cstruct.lenv iovecs in
    let bytes = Bytes.create len in
    let pos = ref 0 in
    List.iter (function
        | { Cstruct.buffer; off; len } ->
          Bigstringaf.unsafe_blit_to_bytes buffer ~src_off:off bytes ~dst_off:!pos ~len;
          pos := !pos + len)
      iovecs;
    shift t len;
    assert (not (has_pending_output t));
    Bytes.unsafe_to_string bytes

let serialize_to_cstruct t =
  close t;
  match await_batch t with
  | exception End_of_file -> Cstruct.empty
  | iovecs ->
    let data = Cstruct.concat iovecs in
    shift t (Cstruct.length data);
    assert (not (has_pending_output t));
    data

let drain =
  let rec loop t acc =
    match await_batch t with
    | exception End_of_file -> acc
    | iovecs ->
      let len = Cstruct.lenv iovecs in
      shift t len;
      loop t (len + acc)
  in
  fun t -> loop t 0
