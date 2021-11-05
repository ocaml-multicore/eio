(*
 * Copyright (C) 2021 Thomas Leonard
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

type 'a or_error = ('a, Luv.Error.t) result

exception Luv_error of Luv.Error.t

val or_raise : 'a or_error -> 'a
(** [or_error (Error e)] raises [Luv_error e]. *)

val await : (Eunix.Suspended.state -> ('a -> unit) -> unit) -> 'a
(** [await fn] converts a function using a luv-style callback to one using effects.
    Use it as e.g. [await (fun fibre -> Luv.File.realpath path)].
    Use [fibre] to implement cancellation. *)

(** {1 Time functions} *)

val sleep_until : float -> unit
(** [sleep_until time] blocks until the current time is [time]. *)

(** {1 Low-level wrappers for Luv functions} *)

module File : sig
  type t

  val is_open : t -> bool
  (** [is_open t] is [true] if {!close t} hasn't been called yet. *)

  val close : t -> unit
  (** [close t] closes [t].
      @raise Invalid_arg if [t] is already closed. *)

  val of_luv : sw:Switch.t -> Luv.File.t -> t
  (** [of_luv ~sw fd] wraps [fd] as an open file descriptor.
      This is unsafe if [fd] is closed directly (before or after wrapping it).
      @param sw The FD is closed when [sw] is released, if not closed manually first. *)

  val to_luv : t -> Luv.File.t
  (** [to_luv t] returns the wrapped descriptor.
      This allows unsafe access to the FD.
      @raise Invalid_arg if [t] is closed. *)

  val open_ :
    sw:Switch.t ->
    ?mode:Luv.File.Mode.t list ->
    string -> Luv.File.Open_flag.t list -> t or_error
  (** Wraps {!Luv.File.open_} *)

  val read : t -> Luv.Buffer.t list -> Unsigned.Size_t.t or_error
  (** Wraps {!Luv.File.read} *)

  val write : t -> Luv.Buffer.t list -> unit
  (** [write t bufs] writes all the data in [bufs] (which may take several calls to {!Luv.File.write}). *)

  val realpath : string -> string or_error
  (** Wraps {!Luv.File.realpath} *)

  val mkdir : mode:Luv.File.Mode.t list -> string -> unit or_error
  (** Wraps {!Luv.File.mkdir} *)
end

module Handle : sig
  type 'a t

  val is_open : 'a t -> bool
  (** [is_open t] is [true] if {!close t} hasn't been called yet. *)

  val close : 'a t -> unit
  (** [close t] closes [t].
      @raise Invalid_arg if [t] is already closed. *)

  val to_luv : 'a t -> 'a Luv.Handle.t
  (** [to_luv t] returns the wrapped handle.
      This allows unsafe access to the handle.
      @raise Invalid_arg if [t] is closed. *)

  val of_luv : sw:Switch.t -> 'a Luv.Handle.t -> 'a t
  (** [of_luv ~sw h] wraps [h] as an open handle.
      This is unsafe if [h] is closed directly (before or after wrapping it).
      @param sw The handle is closed when [sw] is released, if not closed manually first. *)
end

(** {1 Eio API} *)

module Objects : sig
  type has_fd = < fd : File.t >
  type source = < Eio.Flow.source; Eio.Flow.close; has_fd >
  type sink   = < Eio.Flow.sink  ; Eio.Flow.close; has_fd >

  type stdenv = <
    stdin  : source;
    stdout : sink;
    stderr : sink;
    net : Eio.Net.t;
    domain_mgr : Eio.Domain_manager.t;
    clock : Eio.Time.clock;
    fs : Eio.Dir.t;
    cwd : Eio.Dir.t;
  >

  val get_fd : <has_fd; ..> -> File.t
  val get_fd_opt : #Eio.Generic.t -> File.t option
end

(** {1 Main Loop} *)

val run : (Objects.stdenv -> unit) -> unit
