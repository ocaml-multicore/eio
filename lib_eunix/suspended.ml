open EffectHandlers.Deep

type 'a t = {
  fibre : Eio.Private.context;
  k : ('a, [`Exit_scheduler]) continuation;
}

let continue t v =
  Ctf.note_switch t.fibre.tid;
  continue t.k v

let discontinue t ex =
  Ctf.note_switch t.fibre.tid;
  discontinue t.k ex
