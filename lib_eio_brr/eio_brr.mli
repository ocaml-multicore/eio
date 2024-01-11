(** {1 Eio scheduler setup} *)

val start : (unit -> unit) -> unit
(** [start f] executes function [f] asynchronously in a context where
    Eio operations can be performed.

    This function is an alias for {!Eio_js_scheduler.start}.
*)

(** {1 Eio variants of Brr functions} *)

(** {2:data Data containers and encodings} *)

(** Blob objects. *)
module Blob : sig
  val array_buffer : Brr.Blob.t -> Brr.Tarray.Buffer.t
  (** [array_buffer b] is an
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Blob/arrayBuffer}
      array buffer} with the contents of [b]. *)

  val text : Brr.Blob.t -> Jstr.t
  (** [text b] is the
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Blob/text}string}
      that results from UTF-8 decoding the contents of [b]. *)

  val data_uri : Brr.Blob.t -> Jstr.t
  (** [data_uri b] is [b] as a data URI (via the
      {{:https://developer.mozilla.org/en-US/docs/Web/API/FileReader}
      [FileReader]} API). *)
end

(** {2 DOM interaction} *)

(** DOM events. *)
module Ev : sig
  val next : ?capture:bool -> 'a Brr.Ev.type' -> Brr.Ev.target -> 'a Brr.Ev.t
  (** [next type' t] returns the next event of type [type'] on target [t]. *)

  (** [DataTransfer] objects. *)
  module Data_transfer : sig
    (** [DataTransferItem] objects. *)
    module Item : sig
      val get_jstr : Brr.Ev.Data_transfer.Item.t -> Jstr.t
      (** [get_jstr i] is the item's text. *)
    end
  end

  module Extendable : sig
    val wait_until :
      Brr.Ev.Extendable.t -> sw:Eio.Switch.t -> (unit -> _) -> unit
    (** [wait_until e ~sw fn] {{:https://developer.mozilla.org/en-US/docs/Web/API/ExtendableEvent/waitUntil}indicates} to the event dispatcher that work is ongoing. Function [fn] is run in a new fiber attached to [sw]. The work is considered completed when [fn] returns. *)
  end
end

(** DOM elements. *)
module El : sig
  (** {1 Pointer locking} *)

  val request_pointer_lock : Brr.El.t -> unit
  (** [request_pointer_lock e] requests the pointer to be locked
      to [e] in the document it belongs to. This listens on the
      document for the next [Brr.Ev.pointerlockchange] and
      [Brr.Ev.pointerlockerror] to resolve the future appropriately. *)

  (** {1 Fullscreen} *)

  val request_fullscreen : ?opts:Brr.El.fullscreen_opts -> Brr.El.t -> unit
  (** [request_fullscreen e] requests to make the element
      to be displayed in fullscreen mode. *)
end

(** [Document] objects *)
module Document : sig
  (** {1 Pointer locking} *)

  val exit_pointer_lock : Brr.Document.t -> unit
  (** [exit_pointer_lock d] {{:https://developer.mozilla.org/en-US/docs/Web/API/Document/exitPointerLock}exits} pointer lock mode. This returns
      when the corresponding [Brr.Ev.pointerlockchange] on [d] has fired. *)

  (** {1 Fullscreen}

      Use {!El.request_fullscreen} to get into fullscreen mode. *)

  val exit_fullscreen : Brr.Document.t -> unit
  (** [exit_fullscreen d]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Document/exitFullscreen}exits} fullscreen mode. *)
end

(** The global object, its global objects and functions. *)
module G : sig
  val set_timeout : ms:int -> unit
  (** [set_timeout ~ms] is a timer waiting for [ms] milliseconds. It
      can be cancelled. *)

  val request_animation_frame : unit -> float
  (** [request_animation_frame ()]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Window/requestAnimationFrame}waits}
      until right before the next repaint. It returns the current
      point in time. It can be cancelled. *)
end
