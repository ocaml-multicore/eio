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

 (** Wrap [Unix.file_descr] to track whether it has been closed. *)
 module FD : sig
   type t
 
   val is_open : t -> bool
   (** [is_open t] is [true] if {!close t} hasn't been called yet. *)
 
   val close : t -> unit
   (** [close t] closes [t].
       @raise Invalid_arg if [t] is already closed. *)
 
   val of_unix : Unix.file_descr -> t
   (** [of_unix fd] wraps [fd] as an open file descriptor.
       This is unsafe if [fd] is closed directly (before or after wrapping it). *)
 
   val to_unix : t -> Unix.file_descr
   (** [to_unix t] returns the wrapped descriptor.
       This allows unsafe access to the FD.
       @raise Invalid_arg if [t] is closed. *)
 end

 (** {1 File manipulation functions} *)
 
 val openfile : string -> Unix.open_flag list -> int -> FD.t
 (** Like {!Unix.open_file}. *)
 
 val read_upto : file_offset:int -> FD.t -> Dispatch.Data.t ref -> int -> int
 (** [read_upto fd chunk len] reads at most [len] bytes from [fd],
     returning as soon as some data is available.
     @param file_offset Read from the given position in [fd] (default: 0).
     @raise End_of_file Raised if all data has already been read. *)
 
 val write : file_offset:int -> FD.t -> Dispatch.Data.t ref -> int -> unit
 (** [write fd buf len] writes exactly [len] bytes from [buf] to [fd].
     It blocks until the OS confirms the write is done,
     and resubmits automatically if the OS doesn't write all of it at once. *)
 
 (** {1 Eio API} *)
 
 module Objects : sig
    type _ Eio.Generic.ty += FD : FD.t Eio.Generic.ty
   type source = < Eio.Flow.source; Eio.Flow.close; fd : FD.t >
   type sink   = < Eio.Flow.sink  ; Eio.Flow.close; fd : FD.t >
 
   type stdenv = <
    stdin : source;
    stdout : sink;
    stderr : sink;
    net : Eio.Net.t;
    domain_mgr : Eio.Domain_manager.t;
    clock : Eio.Time.clock;
    fs : Eio.Dir.t;
    cwd : Eio.Dir.t;
  >
 end
 
 val pipe : unit -> Objects.source * Objects.sink
 (** [pipe ()] is a source-sink pair [(r, w)], where data written to [w] can be read from [r].
     It is implemented as a Unix pipe. *)
 
 (** {1 Main Loop} *)
 
 val run : (Objects.stdenv -> unit) -> unit
 (** FIXME queue_depth and block_size should be in a handler and not the mainloop *)
 