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

  module Effects = struct
    type 'a enqueue = 'a Suspend.enqueue
    type _ Effect.t +=
      | Suspend = Suspend.Suspend
      | Fork = Fiber.Fork
      | Get_context = Cancel.Get_context
      | Trace : (?__POS__:(string * int * int * int) -> ('a, Format.formatter, unit, unit) format4 -> 'a) Effect.t
  end

  let traceln_mutex = Mutex.create ()

  let default_traceln ?__POS__:pos fmt =
    let k go =
      let b = Buffer.create 512 in
      let f = Format.formatter_of_buffer b in
      go f;
      Option.iter (fun (file, lnum, _, _) -> Format.fprintf f " [%s:%d]" file lnum) pos;
      Format.pp_close_box f ();
      Format.pp_print_flush f ();
      let msg = Buffer.contents b in
      Ctf.label msg;
      let lines = String.split_on_char '\n' msg in
      Mutex.lock traceln_mutex;
      Fun.protect ~finally:(fun () -> Mutex.unlock traceln_mutex) @@ fun () ->
      List.iter (Printf.eprintf "+%s\n") lines;
      flush stderr
    in
    Format.kdprintf k ("@[" ^^ fmt)

end
