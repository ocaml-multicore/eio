(** Clipboard access

    See the {{:https://developer.mozilla.org/en-US/docs/Web/API/Clipboard}
    Clipboard API}. *)
module Clipboard : sig
  (** Clipboard items. *)
  module Item : sig
    val get_type : Brr_io.Clipboard.Item.t -> Jstr.t -> Brr.Blob.t
    (** [get_type i t] is the {{:https://developer.mozilla.org/en-US/docs/Web/API/ClipboardItem/getType}blob object} with MIME type [t] for item [i]. *)
  end

  (** {1:rw Reading and writing} *)

  val read : Brr_io.Clipboard.t -> Brr_io.Clipboard.Item.t list
  (** [read c] is the {{:https://developer.mozilla.org/en-US/docs/Web/API/Clipboard/read}content} of [c]. *)

  val read_text : Brr_io.Clipboard.t -> Jstr.t
  (** [read_text c] is the clipboard {{:https://developer.mozilla.org/en-US/docs/Web/API/Clipboard/readText}textual content} of [c]. *)

  val write : Brr_io.Clipboard.t -> Brr_io.Clipboard.Item.t list -> unit
  (** [write c is]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Clipboard/write}
      writes} the items [is] to [c]. *)

  val write_text : Brr_io.Clipboard.t -> Jstr.t -> unit
  (** [write_text c s]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Clipboard/writeText}
      writes} the string [s] to [c]. *)
end

(** Fetching resources.

    See the {{:https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API}
    Fetch API}. *)
module Fetch : sig
  (** Body specification and interface. *)
  module Body : sig
    val array_buffer :
      ?abort:Brr.Abort.t -> Brr_io.Fetch.Body.t -> Brr.Tarray.Buffer.t
    (** [array_buffer ?abort b]
        {{:https://developer.mozilla.org/en-US/docs/Web/API/Body/arrayBuffer}
        reads} [b] into an array buffer. If the fiber is cancelled,
        the optional controller [abort] is used to cancel the fetch
        operation. *)

    val blob : ?abort:Brr.Abort.t -> Brr_io.Fetch.Body.t -> Brr.Blob.t
    (** [blob ?abort b]
        {{:https://developer.mozilla.org/en-US/docs/Web/API/Body/blob}
        reads} [b] as a blob. If the fiber is cancelled, the optional
        controller [abort] is used to cancel the fetch operation. *)

    val form_data :
      ?abort:Brr.Abort.t -> Brr_io.Fetch.Body.t -> Brr_io.Form.Data.t
    (** [form_data ?abort b]
        {{:https://developer.mozilla.org/en-US/docs/Web/API/Body/formData}
        reads} [b] as form data. If the fiber is cancelled, the
        optional controller [abort] is used to cancel the fetch
        operation. *)

    val json : ?abort:Brr.Abort.t -> Brr_io.Fetch.Body.t -> Brr.Json.t
    (** [json ?abort b]
        {{:https://developer.mozilla.org/en-US/docs/Web/API/Body/json}
        reads} [b] and parses it as JSON data. If the fiber is
        cancelled, the optional controller [abort] is used to cancel
        the fetch operation. *)

    val text : ?abort:Brr.Abort.t -> Brr_io.Fetch.Body.t -> Jstr.t
    (** [text ?abort b]
        {{:https://developer.mozilla.org/en-US/docs/Web/API/Body/text}reads}
        [b] and UTF-8 decodes it to a string. If the fiber is
            cancelled, the optional controller [abort] is used to cancel
            the fetch operation. *)
  end

  (** Fetch caches. *)
  module Cache : sig
    val match' :
      ?query_opts:Brr_io.Fetch.Cache.query_opts ->
      Brr_io.Fetch.Cache.t ->
      Brr_io.Fetch.Request.t ->
      Brr_io.Fetch.Response.t option
    (** [match' c req] is a {{:https://developer.mozilla.org/en-US/docs/Web/API/Cache/match}stored response} for [req] in [c] (if any). *)

    val match_all :
      ?query_opts:Brr_io.Fetch.Cache.query_opts ->
      Brr_io.Fetch.Cache.t ->
      Brr_io.Fetch.Request.t ->
      Brr_io.Fetch.Response.t list
    (** [match_all c req] is a list {{:https://developer.mozilla.org/en-US/docs/Web/API/Cache/matchAll}stored response} for [req] in [c]. *)

    val add : Brr_io.Fetch.Cache.t -> Brr_io.Fetch.Request.t -> unit
    (** [add c req] fetches [req] and
        {{:https://developer.mozilla.org/en-US/docs/Web/API/Cache/add}adds}
        the response to [c]. *)

    val add_all : Brr_io.Fetch.Cache.t -> Brr_io.Fetch.Request.t list -> unit
    (** [add_all c reqs] fetches [reqs] and
        {{:https://developer.mozilla.org/en-US/docs/Web/API/Cache/addAll}adds}
        their reponses to [c]. *)

    val put :
      Brr_io.Fetch.Cache.t ->
      Brr_io.Fetch.Request.t ->
      Brr_io.Fetch.Response.t ->
      unit
    (** [put c req resp]
        {{:https://developer.mozilla.org/en-US/docs/Web/API/Cache/put}puts}
        the [req]/[resp] pair to the cache. *)

    val delete :
      ?query_opts:Brr_io.Fetch.Cache.query_opts ->
      Brr_io.Fetch.Cache.t ->
      Brr_io.Fetch.Request.t ->
      bool
    (** [delete c req] {{:https://developer.mozilla.org/en-US/docs/Web/API/Cache/delete}deletes} response to [req] from the cache. [false]
        is returned if [req] was not in the cache. *)

    val keys :
      ?query_opts:Brr_io.Fetch.Cache.query_opts ->
      ?req:Brr_io.Fetch.Request.t ->
      Brr_io.Fetch.Cache.t ->
      Brr_io.Fetch.Request.t list
    (** [keys c] are the {{:https://developer.mozilla.org/en-US/docs/Web/API/Cache/keys}requests} cached by [c]. *)

    (** {1:cache_storage Cache storage} *)

    (** Cache storage objects. *)
    module Storage : sig
      val match' :
        ?query_opts:Brr_io.Fetch.Cache.query_opts ->
        Brr_io.Fetch.Cache.Storage.t ->
        Brr_io.Fetch.Request.t ->
        Brr_io.Fetch.Response.t option
      (** [match' s req] is a {{:https://developer.mozilla.org/en-US/docs/Web/API/CacheStorage/match}stored response} for [req] in [s] (if any). *)

      val has : Brr_io.Fetch.Cache.Storage.t -> Jstr.t -> bool
      (** [has s n] is [true] if [n] matches a {{:https://developer.mozilla.org/en-US/docs/Web/API/CacheStorage/has}cache name} in [s]. *)

      val open' :
        Brr_io.Fetch.Cache.Storage.t ->
        Jstr.t ->
        Brr_io.Fetch.Cache.Storage.cache
      (** [open' s n] {{:https://developer.mozilla.org/en-US/docs/Web/API/CacheStorage/open}opens} the cache named [n] of [s]. *)

      val delete : Brr_io.Fetch.Cache.Storage.t -> Jstr.t -> bool
      (** [delete s n] {{:https://developer.mozilla.org/en-US/docs/Web/API/CacheStorage/delete}deletes} the cache named [n] from [s]. [false] is returned
          if [n] did not exist. *)

      val keys : Brr_io.Fetch.Cache.Storage.t -> Jstr.t list
      (** [keys s] are the {{:https://developer.mozilla.org/en-US/docs/Web/API/CacheStorage/keys}cache names} in [s]. *)
    end
  end

  (** Fetch events. *)
  module Ev : sig
    val preload_response : Brr_io.Fetch.Ev.t -> Brr_io.Fetch.Response.t option
    (** [preload_response e] is a navigation response {{:https://developer.mozilla.org/en-US/docs/Web/API/FetchEvent/preloadResponse}preload} (if any). *)

    val handled : Brr_io.Fetch.Ev.t -> unit
    (** [handled e] is obscure. *)

    val respond_with :
      Brr_io.Fetch.Ev.t ->
      sw:Eio.Switch.t ->
      (unit -> Brr_io.Fetch.Response.t) ->
      unit
    (** [respond_with e ~sw fn] replace the browser's default fetch handling
        with the {{:https://developer.mozilla.org/en-US/docs/Web/API/FetchEvent/respondWith}
        response} [fn []]. Function [fn] is run in a new fiber attached to [sw]. *)
  end

  val url :
    ?abort:Brr.Abort.t ->
    ?init:Brr_io.Fetch.Request.init ->
    Jstr.t ->
    Brr_io.Fetch.Response.t
  (** [url ?abort ~init u]
        {{:https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/fetch}fetches}
        URL [u] with the [init] request object. If the fiber is
        cancelled, the optional controller [abort] is used to cancel
        the fetch operation. *)

  val request :
    ?abort:Brr.Abort.t -> Brr_io.Fetch.Request.t -> Brr_io.Fetch.Response.t
  (** [request ?abort r]
          {{:https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/fetch}fetches}
          request [r]. If the fiber is cancelled, the optional
          controller [abort] is used to cancel the fetch operation. *)

  (**
     Here is an example of how to use this API:
     {[
       let fetch url =
         let abort = Brr.Abort.controller () in
         let init = Brr_io.Fetch.Request.init ~signal:(Brr.Abort.signal abort) () in
         let response = Eio_brr_io.Fetch.url ~abort ~init url in
         Eio_brr_io.Fetch.Body.text ~abort (Brr_io.Fetch.Response.as_body response)
     ]}
  *)
end

(** Access to device location.

    See {{:https://developer.mozilla.org/en-US/docs/Web/API/Geolocation_API}
    Geolocation API}. *)
module Geolocation : sig
  val get :
    ?opts:Brr_io.Geolocation.opts ->
    Brr_io.Geolocation.t ->
    (Brr_io.Geolocation.Pos.t, Brr_io.Geolocation.Error.t) result
  (** [get l ~opts] is the position of [l]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Geolocation/getCurrentPosition}determined}
      with options [opts]. *)
end

module Media : sig
  (** {1:media Media devices, streams and tracks} *)

  (** Media stream tracks. *)
  module Track : sig
    val apply_constraints :
      Brr_io.Media.Track.t -> Brr_io.Media.Constraints.t option -> unit
    (** [apply_contraints t] applies the
        {{:https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamTrack/applyConstraints}applies}
        the given contraints.  Constraints unspecified are restored to
        their default value.  If no contraints are given all
        contraints are restored to their defaults.  *)
  end

  (** Media device enumeration. *)
  module Devices : sig
    val enumerate : Brr_io.Media.Devices.t -> Brr_io.Media.Device.Info.t list
    (** [enumerate m]
    {{:https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices/enumerateDevices}determines}
    a list of connected media devices. Monitor changes by listening
    {!Ev.devicechange} on [m]. *)

    val get_user_media :
      Brr_io.Media.Devices.t ->
      Brr_io.Media.Stream.Constraints.t ->
      Brr_io.Media.Stream.t
    (** [get_user_media m c]
        {{:https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices/getUserMedia}prompts}
        the user to use a media input which can produce a media stream
        constrained by [c].
        {{:https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices/getUserMedia#Exceptions}These
        errors} can occur. In particular [Jv.Error.Not_allowed] and
        [Jv.Error.Not_found] should be reported to the user in a
        friendly way. In some browsers this call has to done
        in a user interface event handler. *)

    val get_display_media :
      Brr_io.Media.Devices.t ->
      Brr_io.Media.Stream.Constraints.t ->
      Brr_io.Media.Stream.t
    (** [get_display_media m c]
        {{:https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices/getDisplayMedia}prompts} the user to select and grant permission to capture the
        contents of a display as a media stream. A video
        track is unconditionally returned even if [c] says otherwise.
        In some browsers this call has to done in a user interface event
        handler.

        See this
        {{:https://developer.mozilla.org/en-US/docs/Web/API/Screen_Capture_API/Using_Screen_Capture}MDN article} for more details. *)
  end

  (** {1:el Media element interface} *)

  (** The HTML {{:https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElem
ent}media element interface}.

      {b Warning.} This binding is incomplete, the modules
      {!El.Audio_track}, {!El.Video_track}, {!El.Text_track} are mostly
      empty. *)
  module El : sig
    val play : Brr_io.Media.El.t -> unit
    (** [play m]
        {{:https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/play}plays} [m]. *)
  end
end

(** Notifying users.

    See the {{:https://developer.mozilla.org/en-US/docs/Web/API/Notifications_AP
I}Notification API}. *)
module Notification : sig
  (** {1:perm Permission} *)

  val request_permission : unit -> Brr_io.Notification.Permission.t
  (** [request_permission ()] {{:https://developer.mozilla.org/en-US/docs/Web/API/Notification/requestPermission}requests} permission to display
      notifications. *)
end
