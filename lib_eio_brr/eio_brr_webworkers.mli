(** Web and Service Worker APIs. *)

open Brr_webworkers

(** Service workers.

    See the {{:https://developer.mozilla.org/en-US/docs/Web/API/Service_Worker_API}Service Worker API}.

    The fetch caches and events are in {!Brr_io.Fetch}. *)
module Service_worker : sig
  (** Ressources preloading *)
  module Navigation_preload_manager : sig
    val enable : Service_worker.Navigation_preload_manager.t -> unit
    (** [enable p] {{:https://developer.mozilla.org/en-US/docs/Web/API/NavigationPreloadManager#Methods}enables} navigation preloading. *)

    val disable : Service_worker.Navigation_preload_manager.t -> unit
    (** [disables p] {{:https://developer.mozilla.org/en-US/docs/Web/API/NavigationPreloadManager#Methods}disables} navigation preloading. *)

    val set_header_value :
      Service_worker.Navigation_preload_manager.t -> Jstr.t -> unit
    (** [set_header_value p v] {{:https://developer.mozilla.org/en-US/docs/Web/API/NavigationPreloadManager#Methods}sets} the value of the header. *)

    val get_state : Service_worker.Navigation_preload_manager.t -> bool * Jstr.t
    (** [get_state p] {{:https://developer.mozilla.org/en-US/docs/Web/API/NavigationPreloadManager#Methods}indicates} whether preload is enabled and
        the value of the header. *)
  end

  (** Service registration objects. *)
  module Registration : sig
    val update : Service_worker.Registration.t -> unit
    (** [update r] attempts to {{:https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerRegistration/update}update} the service worker of [r]. *)

    val unregister : Service_worker.Registration.t -> bool
    (** [unregister r]
        {{:https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerRegistration/unregister}unregisters} the service worker registration. This is [false]
        if no registration was false. *)

    val show_notification :
      ?opts:Brr_io.Notification.opts ->
      Service_worker.Registration.t ->
      Jstr.t ->
      unit
    (** [show_notification r title ~opts]
        {{:https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerRegistration/showNotification}displays} a notification with title [title] an options
        [opts]. *)

    val get_notifications :
      ?tag:Jstr.t -> Service_worker.Registration.t -> Brr_io.Notification.t list
    (** [get_notifications r ~tag] are {{:https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerRegistration/getNotifications}notifications} created via [r] and tagged with [tag] (or all of them if unspecified). *)
  end

  (** Service worker containers. *)
  module Container : sig
    val ready : Service_worker.Container.t -> Service_worker.Registration.t
    (** [ready c] is a future that resolves when a service worker is
        {{:https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerContainer/ready}active}. *)

    val register :
      ?register_opts:Service_worker.Container.register_opts ->
      Service_worker.Container.t ->
      Jstr.t ->
      Service_worker.Registration.t
    (** [register c script_uri ~register_opts] {{:https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerContainer/register}creates or updates} a
        registration with [script_url]. *)

    val get_registration :
      Service_worker.Container.t ->
      Jstr.t option ->
      Service_worker.Registration.t option
    (** [get_registration c url] is the {{:https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerContainer/getRegistration}registration} for
        [url] (if any). *)

    val get_registrations :
      Service_worker.Container.t -> Service_worker.Registration.t list
    (** [get_registrations c] are {{:https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerContainer/getRegistrations}all} the registration fo [c]. *)
  end

  (** {1:worker_funs Service worker context}

      These APIs are used by the service worker. *)

  (** Client objects. *)
  module Client : sig
    (** {1:window Window clients} *)

    (** Window clients. *)
    module Window : sig
      val focus :
        Service_worker.Client.Window.t -> Service_worker.Client.Window.t
      (** [focus w] {{:https://developer.mozilla.org/en-US/docs/Web/API/WindowClient/focus}focuses} [w]. *)

      val navigate :
        Service_worker.Client.Window.t ->
        Jstr.t ->
        Service_worker.Client.Window.t
      (** [navigate w uri] {{:https://developer.mozilla.org/en-US/docs/Web/API/WindowClient/navigate}loads} [uri] in [w]. *)
    end
  end

  (** Clients objects. *)
  module Clients : sig
    val get :
      Service_worker.Clients.t -> Jstr.t -> Service_worker.Client.t option
    (** [get cs id] is a client {{:https://developer.mozilla.org/en-US/docs/Web/API/Clients/get}matching} [id] (if any). *)

    val match_all :
      ?query_opts:Service_worker.Clients.query_opts ->
      Service_worker.Clients.t ->
      Service_worker.Client.t list
    (** [match_all cs ~query_opts] are clients {{:https://developer.mozilla.org/en-US/docs/Web/API/Clients/matchAll}matching} [query_opts]. *)

    val open_window :
      Service_worker.Clients.t ->
      Jstr.t ->
      Service_worker.Client.Window.t option
    (** [open_window cs uri] {{:https://developer.mozilla.org/en-US/docs/Web/API/Clients/openWindow}opens} a window on [uri]. *)

    val claim : Service_worker.Clients.t -> unit
    (** [claim cs]
        {{:https://developer.mozilla.org/en-US/docs/Web/API/Clients/claim}sets}
        the calling service worker as a controller for all clients in
        its scope. *)
  end

  (** Service worker global properties and functions. *)
  module G : sig
    val skip_waiting : unit -> unit
    (** [skip_waiting ()]
        {{:https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerGlobalScope/skipWaiting}forces} the waiting service to become
        the active service worker. *)
  end
end
