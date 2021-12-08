open EffectHandlers

type 'a enqueue = ('a, exn) result -> unit
type _ eff += Suspend : (Cancel.fibre_context -> 'a enqueue -> unit) -> 'a eff

let enter_unchecked fn = perform (Suspend fn)

let enter fn =
  enter_unchecked @@ fun fibre enqueue ->
  match Cancel.Fibre_context.get_error fibre with
  | None -> fn fibre enqueue
  | Some ex -> enqueue (Error ex)
