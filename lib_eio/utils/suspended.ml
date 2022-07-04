(** A suspended fiber with its context. *)

open Effect.Deep
module Ctf = Eio.Private.Ctf

type 'a t = {
  fiber : Eio.Private.Fiber_context.t;
  k : ('a, [`Exit_scheduler]) continuation;
}

let tid t = Eio.Private.Fiber_context.tid t.fiber

let continue t v =
  Ctf.note_switch (tid t);
  continue t.k v

let discontinue t ex =
  Ctf.note_switch (tid t);
  discontinue t.k ex
