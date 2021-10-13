open EffectHandlers

type 'a enqueue = ('a, exn) result -> unit
type _ eff += Suspend : (Ctf.id -> 'a enqueue -> unit) -> 'a eff

let enter fn = perform (Suspend fn)
