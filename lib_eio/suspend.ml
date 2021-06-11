type 'a enqueue = ('a, exn) result -> unit
effect Suspend : (Ctf.id -> 'a enqueue -> unit) -> 'a
let enter fn = perform (Suspend fn)
