type 'a enqueue = ('a, exn) result -> unit
type _ Effect.t += Suspend : (Cancel.fiber_context -> 'a enqueue -> unit) -> 'a Effect.t

let enter_unchecked op fn =
  Trace.suspend_fiber op;
  Effect.perform (Suspend fn)

let enter op fn =
  enter_unchecked op @@ fun fiber enqueue ->
  match Cancel.Fiber_context.get_error fiber with
  | None -> fn fiber enqueue
  | Some ex -> enqueue (Error ex)
