(* This module also checks that Eio doesn't pull in a dependency on Unix.
   See the [dune] file. *)

module Tracing = Eio.Private.Tracing
