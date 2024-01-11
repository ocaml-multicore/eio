module Midi = struct
  module Port = struct
    let open' p = Eio_fut.await_exn (Brr_webmidi.Midi.Port.open' p)
    let close p = Eio_fut.await_exn (Brr_webmidi.Midi.Port.close p)
  end

  module Access = struct
    let of_navigator ?opts n =
      Eio_fut.await_exn (Brr_webmidi.Midi.Access.of_navigator ?opts n)
  end
end
