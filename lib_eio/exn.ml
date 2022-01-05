type with_bt = exn * Printexc.raw_backtrace

exception Multiple of exn list  (* Note: the last exception in list is the first one reported *)

exception Cancelled of exn

exception Cancel_hook_failed of exn list

let () =
  Printexc.register_printer @@ function
  | Multiple exns -> Some ("Multiple exceptions:\n" ^ String.concat "\nand\n" (List.rev_map Printexc.to_string exns))
  | Cancel_hook_failed exns -> Some ("During cancellation:\n" ^ String.concat "\nand\n" (List.map Printexc.to_string exns))
  | Cancelled ex -> Some ("Cancelled: " ^ Printexc.to_string ex)
  | _ -> None

let combine e1 e2 =
  if fst e1 == fst e2 then e1
  else match e1, e2 with
    | (Cancelled _, _), e
    | e, (Cancelled _, _) -> e  (* Don't need to report a cancelled exception if we have something better *)
    | (Multiple exs, _), _ when List.memq (fst e2) exs -> e1   (* Avoid duplicates *)
    | (Multiple exs, bt1), (e2, _) -> Multiple (e2 :: exs), bt1
    | (e1, bt1), (e2, _) -> Multiple [e2; e1], bt1
