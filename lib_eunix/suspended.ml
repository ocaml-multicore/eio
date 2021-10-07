open Obj.Effect_handlers.Deep

type 'a t = {
  tid : Ctf.id;
  k : ('a, [`Exit_scheduler]) continuation;
}

let continue t v =
  Ctf.note_switch t.tid;
  continue t.k v

let discontinue t ex =
  Ctf.note_switch t.tid;
  discontinue t.k ex
