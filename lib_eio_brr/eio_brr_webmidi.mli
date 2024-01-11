(** Web MIDI. *)
module Midi : sig
  (** MIDI port. *)
  module Port : sig
    val open' : Brr_webmidi.Midi.Port.t -> unit
    (** [open' p] {{:https://developer.mozilla.org/en-US/docs/Web/API/MIDIPort/open}opens} the port. *)

    val close : Brr_webmidi.Midi.Port.t -> unit
    (** [close p] {{:https://developer.mozilla.org/en-US/docs/Web/API/MIDIPort/close}closes} the port. *)
  end

  (** MIDI access. *)
  module Access : sig
    val of_navigator :
      ?opts:Brr_webmidi.Midi.Access.opts ->
      Brr.Navigator.t ->
      Brr_webmidi.Midi.Access.t
    (** [of_navigator ?opts n] {{:https://developer.mozilla.org/en-US/docs/Web/API/Navigator/requestMIDIAccess}requests} a MIDI access object. *)
  end
end
