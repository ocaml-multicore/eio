module Action = Action
module Handler = Handler
module Flow = Flow
module Net = Net
module Clock = Clock
module Backend = Backend

type Eio.Exn.Backend.t += Simulated_failure
let () = Eio.Exn.Backend.register_pp (fun f -> function
    | Simulated_failure -> Fmt.string f "Simulated_failure"; true
    | _ -> false
  )
