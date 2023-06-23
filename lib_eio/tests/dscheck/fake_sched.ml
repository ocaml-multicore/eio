let cancel ctx = Eio.Cancel.cancel ctx (Failure "test cancellation")

let run fn =
  let module Fiber_context = Eio__core.Private.Fiber_context in
  let continue_result k = function
    | Ok x -> Effect.Deep.continue k x
    | Error x -> Effect.Deep.discontinue k x
  in
  let fiber = lazy (Fiber_context.make_root ()) in
  Effect.Deep.try_with fn ()
      { effc = fun (type a) (e : a Effect.t) : ((a, 'b) Effect.Deep.continuation -> 'b) option ->
          match e with
          | Eio.Private.Effects.Suspend fn ->
            Some (fun cont ->
                fn (Lazy.force fiber) (continue_result cont);
            )
          | _ -> None
      };
  if Lazy.is_val fiber then
    Some (Fiber_context.cancellation_context (Lazy.force fiber))
  else None
