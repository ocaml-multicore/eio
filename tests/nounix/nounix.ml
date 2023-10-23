(* This module checks that Eio doesn't pull in a dependency on Unix.
   See the [dune] file. *)

let () =
  assert (Eio.Buf_read.(parse_string_exn take_all) "hi" = "hi")
