let start = Eio_js_backend.start

module Blob = struct
  let array_buffer b = Eio_fut.await_exn (Brr.Blob.array_buffer b)
  let text b = Eio_fut.await_exn (Brr.Blob.text b)
  let data_uri b = Eio_fut.await_exn (Brr.Blob.data_uri b)
end

module Ev = struct
  let next ?capture typ target = Eio_fut.await (Brr.Ev.next ?capture typ target)

  module Data_transfer = struct
    module Item = struct
      let get_jstr i = Eio_fut.await (Brr.Ev.Data_transfer.Item.get_jstr i)
    end
  end

  module Extendable = struct
    let wait_until e ~sw f =
      Brr.Ev.Extendable.wait_until e (Eio_fut.make_exn ~sw f)
  end
end

module El = struct
  let request_pointer_lock e = Eio_fut.await_exn (Brr.El.request_pointer_lock e)

  let request_fullscreen ?opts e =
    Eio_fut.await_exn (Brr.El.request_fullscreen ?opts e)
end

module Document = struct
  let exit_pointer_lock e = Eio_fut.await (Brr.Document.exit_pointer_lock e)
  let exit_fullscreen e = Eio_fut.await_exn (Brr.Document.exit_fullscreen e)
end

module G = struct
  let set_timeout ~ms =
    Eio_js_backend.await
      ~setup:(fun ~resolve ~reject:_ -> Brr.G.set_timeout ~ms resolve)
      ~cancel:Brr.G.stop_timer

  let request_animation_frame () =
    Eio_js_backend.await
      ~setup:(fun ~resolve ~reject:_ -> Brr.G.request_animation_frame resolve)
      ~cancel:Brr.G.cancel_animation_frame
end
