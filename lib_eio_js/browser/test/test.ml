(* Avoiding dependency on js_of_ocaml *)
external set_channel_output' :
  out_channel -> Jv.t -> unit
  = "caml_ml_set_channel_output"

let set_channel_flusher (out_channel : out_channel) (f : string -> unit) =
  let f' =
    Jv.callback ~arity:1 (fun s -> f (Jstr.binary_to_octets s))
  in
  set_channel_output' out_channel f'

let init () =
  let open Brr in
  let parser = Ansi.create () in
  let output =
    let out = El.div [ El.h1 [ El.txt' "Eio_browser" ] ] in
    El.set_at (Jstr.v "style")
      (Some
         (Jstr.v
            "font-family: monospace; color: #e8e8e8; background: #131212; \
             padding: 2em;"))
      out;
    El.(set_prop Prop.id (Jstr.v "output") out);
    let style = El.style [ El.txt' Ansi.css ] in
    El.append_children (Document.body G.document) [ style; out ];
    out
  in
  let get_or_make name =
    match Document.find_el_by_id G.document (Jstr.v name) with
    | Some v -> v
    | None ->
      let d = El.div [] in
      El.append_children output [ d ];
      El.(set_prop Prop.id (Jstr.v name) d);
      d
  in
  let append name s =
    Brr.Console.log [ s ];
    let s = Ansi.process parser s in
    let p = El.pre [] in
    El.to_jv p |> fun jv ->
    Jv.set jv "innerHTML" (Jv.of_string s);
    El.append_children (get_or_make name) [ p ]
  in
  set_channel_flusher stdout (fun content -> append "stdout" content);
  set_channel_flusher stderr (fun content -> append "stderr" content);
  ()

module Browser_tests = struct
  open Eio

  let test_timeout_cancel () =
    let v =
      Fiber.first
        (fun () -> Eio_browser.Timeout.sleep ~ms:5000; "A")
        (fun () -> "B")
    in
    Alcotest.(check string) "timeout cancelled" "B" v

  let test_fut_cancel () =
    let p, _ = Fut.create () in
    let v =
      Fiber.first
        (fun () -> Eio_browser.await p; "A")
        (fun () -> "B")
    in
    Alcotest.(check string) "fut cancelled" "B" v

  let tests = [
    Alcotest.test_case "timeout cancelled" `Quick test_timeout_cancel;
    Alcotest.test_case "fut cancelled" `Quick test_fut_cancel
  ]
end


let () =
  init ();
  let main =
    Eio_browser.run @@ fun env ->
    try Alcotest.run "eio" [
        "fibers", Eio_test.Fibers.tests;
        "stream", Eio_test.Stream.tests;
        "browser", Browser_tests.tests
      ] with Exit -> ()
  in
  Fut.await main (fun () -> ())