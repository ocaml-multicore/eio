open Eio.Std
open Brr

let get_element s = Document.find_el_by_id G.document Jstr.(v s) |> Option.get

let () =
  let counter = get_element "counter" in
  let text = get_element "text" in
  let output = get_element "output" in
  Eio_brr.start @@ fun () ->
  Fiber.both
    (fun () ->
      (* A little text editor *)
      while true do
        let ev = Eio_brr.Ev.next Ev.keyup (El.as_target text) in
        let target = Jv.get (Ev.to_jv ev) "target" in
        let text = Jv.get target "value" |> Jv.to_jstr in
        El.set_children output [ El.txt text ]
      done)
    (fun () ->
      (* A little timer counting up *)
      let i = ref 0 in
      while true do
        El.set_children counter [ El.txt' (string_of_int !i ^ "s") ];
        Eio_brr.G.set_timeout ~ms:1000;
        incr i
      done)
