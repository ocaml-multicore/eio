open Eio.Std
open Brr

let () =
  let counter = Brr.Document.find_el_by_id G.document Jstr.(v "counter") |> Option.get in
  let text = Brr.Document.find_el_by_id G.document Jstr.(v "text") |> Option.get in
  let output = Brr.Document.find_el_by_id G.document Jstr.(v "output") |> Option.get in
  let main =
    Eio_browser.run @@ fun () ->
    Fiber.both (fun () ->
        (* A little text editor *)
        while true do
          let ev = Eio_browser.next_event Ev.keyup (El.as_target text) in
          let target = Jv.get (Ev.to_jv ev) "target" in
          let text = Jv.get target "value" |> Jv.to_jstr in
          Brr.El.set_children output [ El.txt text ]
        done
      )
      (fun () ->
         (* A little timer counting up *)
         let i = ref 0 in
         while true do
           Eio_browser.Timeout.sleep ~ms:1000;
           incr i;
           Brr.El.set_children counter [ El.txt' (string_of_int !i ^ "s")]
         done
      )
  in
  Fut.await main (fun _ -> Brr.Console.log [ Jstr.v "Done" ])