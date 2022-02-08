(** A suspended fibre with its context. *)

open Eio.Private.Effect.Deep
module Ctf = Eio.Private.Ctf

type 'a t = {
  fibre : Eio.Private.Fibre_context.t;
  k : ('a, [`Exit_scheduler]) continuation;
}

let tid t = Eio.Private.Fibre_context.tid t.fibre

let continue t v =
  Ctf.note_switch (tid t);
  continue t.k v

let discontinue t ex =
  Ctf.note_switch (tid t);
  discontinue t.k ex
