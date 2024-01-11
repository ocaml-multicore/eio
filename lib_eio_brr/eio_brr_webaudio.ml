module Audio = struct
  module Worklet = struct
    let add_module w url =
      Eio_fut.await_exn (Brr_webaudio.Audio.Worklet.add_module w url)
  end

  module Context = struct
    module Base = struct
      let decode_audio_data t b =
        Eio_fut.await_exn
          (Brr_webaudio.Audio.Context.Base.decode_audio_data t b)
    end

    let resume c = Eio_fut.await_exn (Brr_webaudio.Audio.Context.resume c)
    let suspend c = Eio_fut.await_exn (Brr_webaudio.Audio.Context.suspend c)
    let close c = Eio_fut.await_exn (Brr_webaudio.Audio.Context.close c)

    module Offline = struct
      let start_rendering c =
        Eio_fut.await_exn (Brr_webaudio.Audio.Context.Offline.start_rendering c)

      let suspend c ~secs =
        Eio_fut.await_exn (Brr_webaudio.Audio.Context.Offline.suspend c ~secs)

      let resume c =
        Eio_fut.await_exn (Brr_webaudio.Audio.Context.Offline.resume c)
    end
  end
end
