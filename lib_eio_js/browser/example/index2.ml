open Js_of_ocaml

let eio_callback =
  let t = ref 0 in
  fun _ ->
    incr t;
    let rec aux t =
      let p = Dom_html.createP Dom_html.document in
      p##.innerHTML := Js.string (string_of_int t);
      Dom.appendChild Dom_html.window##.document##.body p;
      Eio_browser.Timeout.sleep ~ms:(t*1000);
      aux t
    in
    aux !t

let () =
  let clickable = Js_of_ocaml.Dom_html.getElementById "clickable" in
  clickable##.onclick :=
    Dom_html.handler (Eio_browser.wrap_callback eio_callback);
  Js_of_ocaml.Firebug.console##log "Running main event loop";
  let main = Eio_browser.run Eio_browser.run_callbacks in
  Fut.await main (fun _ -> Js_of_ocaml.Firebug.console##log "Should not happen")
