exception T of exn list

let () =
  Printexc.register_printer @@ function
  | T exns -> Some ("Multiple exceptions:\n" ^ String.concat "\nand\n" (List.map Printexc.to_string exns))
  | _ -> None
