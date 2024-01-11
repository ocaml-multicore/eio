(** Web Audio API.

    See the {{:https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API}
    Web Audio API}. *)

(** Web Audio. *)
module Audio : sig
  (** Audio worklets, their global scope and processors. *)
  module Worklet : sig
    (** {1:worklets Worklets} *)
    val add_module : Brr_webaudio.Audio.Worklet.t -> Jstr.t -> unit
    (** [add_module w url] {{:https://developer.mozilla.org/en-US/docs/Web/API/Worklet/addModule}adds} module [url] to [w]. *)
  end

  (** Audio contexts. *)
  module Context : sig
    (** {1:base_contexts Base audio contexts} *)

    (** Base audio contexts. *)
    module Base : sig
      (** {1:audio_context Audio contexts} *)

      val decode_audio_data :
        Brr_webaudio.Audio.Context.Base.t ->
        Brr_webaudio.Audio.Buffer.t ->
        Brr_webaudio.Audio.Buffer.t
      (** [decode_audio_data t b]
          {{:https://developer.mozilla.org/en-US/docs/Web/API/BaseAudioContext/decodeAudioData}decodes} the audio data in [b]. *)
    end

    (** {1:audio_context Audio contexts} *)

    val resume : Brr_webaudio.Audio.Context.t -> unit
    (** [resume c] {{:https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/resume}resumes} progression of time in [c]. *)

    val suspend : Brr_webaudio.Audio.Context.t -> unit
    (** [suspend c] {{:https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/suspend}suspend} progression of time in [c]. *)

    val close : Brr_webaudio.Audio.Context.t -> unit
    (** [close c] {{:https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/close}closes} the audio context [c]. *)

    (** {1:offline_context Offline audio contexts} *)

    (** Offline audio contexts. *)
    module Offline : sig
      val start_rendering :
        Brr_webaudio.Audio.Context.Offline.t -> Brr_webaudio.Audio.Buffer.t
      (** [start_rendering c] {{:https://developer.mozilla.org/en-US/docs/Web/API/OfflineAudioContext/startRendering}starts} rendering the audio graph
          and determines with the rendered audio buffer. *)

      val suspend : Brr_webaudio.Audio.Context.Offline.t -> secs:float -> unit
      (** [suspend c] {{:https://developer.mozilla.org/en-US/docs/Web/API/OfflineAudioContext/suspend}suspends} rendering for [secs] seconds. *)

      val resume : Brr_webaudio.Audio.Context.Offline.t -> unit
      (** [resume c] {{:https://developer.mozilla.org/en-US/docs/Web/API/OfflineAudioContext/resume}resumes} rendering. *)
    end
  end
end
