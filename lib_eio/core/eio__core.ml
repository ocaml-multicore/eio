module Promise = Promise
module Fiber = Fiber
module Switch = Switch
module Cancel = Cancel
module Exn = Exn
module Private = struct
  module Suspend = Suspend
  module Waiters = Waiters
  module Ctf = Ctf
  module Fiber_context = Cancel.Fiber_context
  module Debug = Debug

  module Effects = struct
    type 'a enqueue = 'a Suspend.enqueue
    type _ Effect.t +=
      | Suspend = Suspend.Suspend
      | Fork = Fiber.Fork
      | Get_context = Cancel.Get_context
  end
end
