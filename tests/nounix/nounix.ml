(* This module also checks that Eio doesn't pull in a dependency on Unix.
   See the [dune] file. *)

module Trace = Eio.Private.Trace

let () =
  let bs = Cstruct.create 8 in
  Trace.BS.set_int64_le bs.buffer 0 1234L;
  assert (Cstruct.LE.get_uint64 bs 0 = 1234L)
