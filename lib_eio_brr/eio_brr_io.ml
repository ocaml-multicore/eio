module Clipboard = struct
  module Item = struct
    let get_type i t = Eio_fut.await_exn (Brr_io.Clipboard.Item.get_type i t)
  end

  let read c = Eio_fut.await_exn (Brr_io.Clipboard.read c)
  let read_text c = Eio_fut.await_exn (Brr_io.Clipboard.read_text c)
  let write c is = Eio_fut.await_exn (Brr_io.Clipboard.write c is)
  let write_text c s = Eio_fut.await_exn (Brr_io.Clipboard.write_text c s)
end

module Fetch = struct
  let wrap_abort abort =
    Option.map (fun abort () -> Brr.Abort.abort abort) abort

  module Body = struct
    let array_buffer ?abort b =
      Eio_fut.await_exn ?abort:(wrap_abort abort)
        (Brr_io.Fetch.Body.array_buffer b)

    let blob ?abort b =
      Eio_fut.await_exn ?abort:(wrap_abort abort) (Brr_io.Fetch.Body.blob b)

    let form_data ?abort b =
      Eio_fut.await_exn ?abort:(wrap_abort abort)
        (Brr_io.Fetch.Body.form_data b)

    let json ?abort b =
      Eio_fut.await_exn ?abort:(wrap_abort abort) (Brr_io.Fetch.Body.json b)

    let text ?abort b =
      Eio_fut.await_exn ?abort:(wrap_abort abort) (Brr_io.Fetch.Body.text b)
  end

  module Cache = struct
    let match' ?query_opts c req =
      Eio_fut.await_exn (Brr_io.Fetch.Cache.match' ?query_opts c req)

    let match_all ?query_opts c req =
      Eio_fut.await_exn (Brr_io.Fetch.Cache.match_all ?query_opts c req)

    let add c req = Eio_fut.await_exn (Brr_io.Fetch.Cache.add c req)
    let add_all c reqs = Eio_fut.await_exn (Brr_io.Fetch.Cache.add_all c reqs)
    let put c req resp = Eio_fut.await_exn (Brr_io.Fetch.Cache.put c req resp)

    let delete ?query_opts c req =
      Eio_fut.await_exn (Brr_io.Fetch.Cache.delete ?query_opts c req)

    let keys ?query_opts ?req c =
      Eio_fut.await_exn (Brr_io.Fetch.Cache.keys ?query_opts ?req c)

    module Storage = struct
      let match' ?query_opts s req =
        Eio_fut.await_exn (Brr_io.Fetch.Cache.Storage.match' ?query_opts s req)

      let has s n = Eio_fut.await_exn (Brr_io.Fetch.Cache.Storage.has s n)
      let open' s n = Eio_fut.await_exn (Brr_io.Fetch.Cache.Storage.open' s n)
      let delete s n = Eio_fut.await_exn (Brr_io.Fetch.Cache.Storage.delete s n)
      let keys s = Eio_fut.await_exn (Brr_io.Fetch.Cache.Storage.keys s)
    end
  end

  module Ev = struct
    let preload_response e =
      Eio_fut.await_exn (Brr_io.Fetch.Ev.preload_response e)

    let handled e = Eio_fut.await_exn (Brr_io.Fetch.Ev.handled e)

    let respond_with e ~sw fn =
      Brr_io.Fetch.Ev.respond_with e (Eio_fut.make_exn ~sw fn)
  end

  let url ?abort ?init u =
    Eio_fut.await_exn ?abort:(wrap_abort abort) (Brr_io.Fetch.url ?init u)

  let request ?abort r =
    Eio_fut.await_exn ?abort:(wrap_abort abort) (Brr_io.Fetch.request r)
end

module Geolocation = struct
  let get ?opts l = Eio_fut.await (Brr_io.Geolocation.get ?opts l)
end

module Media = struct
  module Track = struct
    let apply_constraints t cstrs =
      Eio_fut.await_exn (Brr_io.Media.Track.apply_constraints t cstrs)
  end

  module Devices = struct
    let enumerate m = Eio_fut.await_exn (Brr_io.Media.Devices.enumerate m)

    let get_user_media m c =
      Eio_fut.await_exn (Brr_io.Media.Devices.get_user_media m c)

    let get_display_media m c =
      Eio_fut.await_exn (Brr_io.Media.Devices.get_display_media m c)
  end

  module El = struct
    let play m = Eio_fut.await_exn (Brr_io.Media.El.play m)
  end
end

module Notification = struct
  let request_permission () =
    Eio_fut.await_exn (Brr_io.Notification.request_permission ())
end
