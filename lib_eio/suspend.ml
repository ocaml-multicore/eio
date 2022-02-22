open Effect

type 'a enqueue = ('a, exn) result -> unit
type _ eff += Suspend : (Cancel.fiber_context -> 'a enqueue -> unit) -> 'a eff

let enter_unchecked fn = perform (Suspend fn)

let enter fn =
  enter_unchecked @@ fun fiber enqueue ->
  match Cancel.Fiber_context.get_error fiber with
  | None -> fn fiber enqueue
  | Some ex -> enqueue (Error ex)
