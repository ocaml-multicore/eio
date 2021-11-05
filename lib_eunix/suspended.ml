open EffectHandlers.Deep

type state = {
  tid : Ctf.id;
  mutable switch : Eio.Std.Switch.t;
}

type 'a t = {
  fibre : state;
  k : ('a, [`Exit_scheduler]) continuation;
}

let continue t v =
  Ctf.note_switch t.fibre.tid;
  continue t.k v

let discontinue t ex =
  Ctf.note_switch t.fibre.tid;
  discontinue t.k ex
