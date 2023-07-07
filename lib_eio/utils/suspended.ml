(** A suspended fiber with its context. *)

open Effect.Deep
module Tracing = Eio.Private.Tracing

type 'a t = {
  fiber : Eio.Private.Fiber_context.t;
  k : ('a, [`Exit_scheduler]) continuation;
}

let tid t = Eio.Private.Fiber_context.tid t.fiber

let continue t v =
  Tracing.note_switch (tid t);
  continue t.k v

let discontinue t ex =
  Tracing.note_switch (tid t);
  discontinue t.k ex
