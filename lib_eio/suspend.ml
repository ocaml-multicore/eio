open EffectHandlers

type context = {
  tid : Ctf.id;
  mutable cancel : Cancel.t;
}

type 'a enqueue = ('a, exn) result -> unit
type _ eff += Suspend : (context -> 'a enqueue -> unit) -> 'a eff

let enter_unchecked fn = perform (Suspend fn)

let enter fn =
  enter_unchecked @@ fun fibre enqueue ->
  match Cancel.get_error fibre.cancel with
  | None -> fn fibre enqueue
  | Some ex -> enqueue (Error ex)
