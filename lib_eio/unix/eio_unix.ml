module Effect = Eio.Private.Effect

module Private = struct
  type _ Eio.Generic.ty += Unix_file_descr : [`Peek | `Take] -> Unix.file_descr Eio.Generic.ty

  module Async = struct
    type t = {
      lock : unit -> unit;
      unlock : unit -> unit;
    }
  end

  type _ Effect.t += 
    | Await_readable : Unix.file_descr -> unit Effect.t
    | Await_writable : Unix.file_descr -> unit Effect.t
    | Get_system_clock : Eio.Time.clock Effect.t
    | Socket_of_fd : Eio.Switch.t * bool * Unix.file_descr -> < Eio.Flow.two_way; Eio.Flow.close > Effect.t
    | Set_async_integration : Async.t option -> unit Effect.t
end

let await_readable fd = Effect.perform (Private.Await_readable fd)
let await_writable fd = Effect.perform (Private.Await_writable fd)

let sleep d =
  Eio.Time.sleep (Effect.perform Private.Get_system_clock) d

let run_in_systhread fn =
  let f fiber enqueue =
    match Eio.Private.Fiber_context.get_error fiber with
    | Some err -> enqueue (Error err)
    | None ->
      let _t : Thread.t = Thread.create (fun () -> enqueue (try Ok (fn ()) with exn -> Error exn)) () in
      ()
  in
  Effect.perform (Eio.Private.Effects.Suspend f)

module FD = struct
  let peek x = Eio.Generic.probe x (Private.Unix_file_descr `Peek)
  let take x = Eio.Generic.probe x (Private.Unix_file_descr `Take)

  let as_socket ~sw ~close_unix fd = Effect.perform (Private.Socket_of_fd (sw, close_unix, fd))
end

module Ipaddr = struct
  let to_unix : _ Eio.Net.Ipaddr.t -> Unix.inet_addr = Obj.magic
  let of_unix : Unix.inet_addr -> _ Eio.Net.Ipaddr.t = Obj.magic
end

module Ctf = Ctf_unix
