module Promise = Promise
module Fibre = Fibre
module Semaphore = Semaphore
module Switch = Switch

let traceln ?__POS__ fmt =
  fmt |> Format.kasprintf (fun msg ->
      Ctf.label msg;
      match __POS__ with
      | Some (file, lnum, _, _) -> Format.printf "%s:%d %s@." file lnum msg
      | None -> Format.printf "%s@." msg
    )

module Fibre_impl = struct
  module Effects = struct
    effect Await = Switch.Await
    effect Fork = Fibre.Fork
    effect Fork_ignore = Fibre.Fork_ignore
    effect Yield = Fibre.Yield
  end
  module Waiters = Waiters
  module Switch = Switch
end
