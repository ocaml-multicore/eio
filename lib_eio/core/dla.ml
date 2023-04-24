let prepare_for_await () =
  let state = Atomic.make `Init in
  let release () =
    if Atomic.get state != `Released then
      match Atomic.exchange state `Released with
      | `Awaiting enqueue -> enqueue (Ok ())
      | _ -> ()
  and await () =
    if Atomic.get state != `Released then
      Suspend.enter @@ fun ctx enqueue ->
      let awaiting = `Awaiting enqueue in
      if Atomic.compare_and_set state `Init awaiting then (
        Cancel.Fiber_context.set_cancel_fn ctx (fun ex ->
            if Atomic.compare_and_set state awaiting `Released then (
              enqueue (Error ex)
            )
          )
      ) else (
        enqueue (Ok ())
      )
  in
  Domain_local_await.{ release; await }
