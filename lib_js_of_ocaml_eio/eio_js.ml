let start = Eio_js_backend.start

let sleep d =
  Eio_js_backend.await
    ~setup:(fun ~resolve ~reject:_ ->
      Js_of_ocaml.Dom_html.setTimeout resolve (d *. 1000.))
    ~cancel:Js_of_ocaml.Dom_html.clearTimeout

let yield () = sleep 0.
