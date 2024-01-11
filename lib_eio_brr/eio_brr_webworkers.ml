open Brr_webworkers

module Service_worker = struct
  module Navigation_preload_manager = struct
    let enable p =
      Eio_fut.await_exn (Service_worker.Navigation_preload_manager.enable p)

    let disable p =
      Eio_fut.await_exn (Service_worker.Navigation_preload_manager.disable p)

    let set_header_value p v =
      Eio_fut.await_exn
        (Service_worker.Navigation_preload_manager.set_header_value p v)

    let get_state p =
      Eio_fut.await_exn (Service_worker.Navigation_preload_manager.get_state p)
  end

  module Registration = struct
    let update r = Eio_fut.await_exn (Service_worker.Registration.update r)

    let unregister r =
      Eio_fut.await_exn (Service_worker.Registration.unregister r)

    let show_notification ?opts r title =
      Eio_fut.await_exn
        (Service_worker.Registration.show_notification ?opts r title)

    let get_notifications ?tag r =
      Eio_fut.await_exn (Service_worker.Registration.get_notifications ?tag r)
  end

  module Container = struct
    let ready c = Eio_fut.await_exn (Service_worker.Container.ready c)

    let register ?register_opts c script_uri =
      Eio_fut.await_exn
        (Service_worker.Container.register ?register_opts c script_uri)

    let get_registration c url =
      Eio_fut.await_exn (Service_worker.Container.get_registration c url)

    let get_registrations c =
      Eio_fut.await_exn (Service_worker.Container.get_registrations c)
  end

  module Client = struct
    module Window = struct
      let focus w = Eio_fut.await_exn (Service_worker.Client.Window.focus w)

      let navigate w uri =
        Eio_fut.await_exn (Service_worker.Client.Window.navigate w uri)
    end
  end

  module Clients = struct
    let get cs id = Eio_fut.await_exn (Service_worker.Clients.get cs id)

    let match_all ?query_opts cs =
      Eio_fut.await_exn (Service_worker.Clients.match_all ?query_opts cs)

    let open_window cs uri =
      Eio_fut.await_exn (Service_worker.Clients.open_window cs uri)

    let claim cs = Eio_fut.await_exn (Service_worker.Clients.claim cs)
  end

  module G = struct
    let skip_waiting () = Eio_fut.await_exn (Service_worker.G.skip_waiting ())
  end
end
