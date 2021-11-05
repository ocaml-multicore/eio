open EffectHandlers

type 'a enqueue = ('a, exn) result -> unit
type _ eff += Suspend : (Ctf.id -> 'a enqueue -> unit) -> 'a eff
type _ eff += Suspend_unchecked : (Ctf.id -> 'a enqueue -> unit) -> 'a eff

let enter fn = perform (Suspend fn)
let enter_unchecked fn = perform (Suspend_unchecked fn)
