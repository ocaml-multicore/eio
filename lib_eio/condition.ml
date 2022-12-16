type t = Broadcast.t

let create () = Broadcast.create ()

let await_generic ?mutex t =
  match
    Suspend.enter_unchecked (fun ctx enqueue ->
        match Fiber_context.get_error ctx with
        | Some ex ->
          Option.iter Eio_mutex.unlock mutex;
          enqueue (Error ex)
        | None ->
          match Broadcast.suspend t (fun () -> enqueue (Ok ())) with
          | None ->
            Option.iter Eio_mutex.unlock mutex
          | Some request ->
            Option.iter Eio_mutex.unlock mutex;
            Fiber_context.set_cancel_fn ctx (fun ex ->
                if Broadcast.cancel request then enqueue (Error ex)
                (* else already succeeded *)
              )
      )
  with
  | () -> Option.iter Eio_mutex.lock mutex
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    Option.iter Eio_mutex.lock mutex;
    Printexc.raise_with_backtrace ex bt

let await t mutex = await_generic ~mutex t
let await_no_mutex t = await_generic t

let broadcast = Broadcast.resume_all
