(* WIP backend for Windows using IOCP *)

let run main =
   let stdenv = object
      method stdin = failwith "Not implemented"
      method stdout = failwith "Not implemented"
      method stderr = failwith "Not implemented"
      method debug = Eio.Private.Debug.v
      method clock = failwith "Not implemented"
      method mono_clock = failwith "Not implemented"
      method net = failwith "Not implemented"
      method domain_mgr = failwith "Not implemented"
      method cwd = failwith "Not implemented"
      method fs = failwith "Not implemented"
      method secure_random = failwith "Not implemented"
   end
   in
   let extra_effects : _ Effect.Deep.effect_handler = {
    effc = (fun _ -> None)
   }
   in
   Sched.with_sched @@ fun sched ->
   Sched.run ~extra_effects sched main stdenv
