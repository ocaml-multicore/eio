module Canvas = struct
  let to_blob ?encode c =
    Eio_fut.await_exn (Brr_canvas.Canvas.to_blob ?encode c)
end
