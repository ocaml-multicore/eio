(* This module checks that Eio doesn't pull in a dependency on Unix.
   See the [dune] file. *)

let () =
  assert (Eio.Buf_read.(parse_string_exn take_all) "hi" = "hi")

open Alcotest
open Eio.Std
open Eio.Process

let test_pipe_creation () =
  Eio_main.run @@ fun env ->
    let proc_mgr = Eio.Stdenv.process_mgr env in
    Switch.run @@ fun sw ->
    let (src, sink) = Eio.Process.pipe ~sw proc_mgr in
    match src, sink with
    | `Source src_ty, `Sink sink_ty ->
        Fmt.pr "Pipe created successfully with Source and Sink types.\n"
    | `Close _, `Close _ ->
        Fmt.pr "Pipe created successfully with Close types.\n"
    | _ -> failwith "Unexpected return types from pipe"

let () =
  let open Alcotest in
  run "Process pipe tests" [
    "pipe_creation", [
      test_case "Pipe creation returns correct constrained types" `Quick test_pipe_creation;
    ];
  ]
