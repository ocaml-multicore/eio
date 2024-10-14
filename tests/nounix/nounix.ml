(* This module checks that Eio doesn't pull in a dependency on Unix.
   See the [dune] file. *)

let () =
  assert (Eio.Buf_read.(parse_string_exn take_all) "hi" = "hi")

o(* Assume the necessary modules are already imported *)

module Flow = struct
  type source_ty = Source
  type sink_ty = Sink
end

module Resource = struct
  type close_ty = Close
end

module Switch = struct
  type t = unit
end

let pipe (_: 'a) ~sw:Switch.t : ([> Flow.source_ty | Resource.close_ty] * [> Flow.sink_ty | Resource.close_ty]) =
  (Flow.Source, Flow.Sink)

let test_pipe_compatibility () =
  let sw = () in (* Mock switch *)

  (* Call the pipe function and expect the original constrained types *)
  let (source, sink) = pipe () ~sw in

  (* Pattern match to assert compatibility with the original constrained types *)
  match source, sink with
  | Flow.Source, Flow.Sink ->
    print_endline "Test passed: Source and Sink are compatible with constrained types."
  | _ ->
    print_endline "Test failed: Types do not match the expected constrained types."

let () = test_pipe_compatibility ()
