type unix_fd = <
  unix_fd : [`Peek | `Take] -> Unix.file_descr;
>

type socket = <
  Eio.Flow.two_way;
  Eio.Flow.close;
  unix_fd;
>

module Private = struct
  type _ Eio.Generic.ty += Unix_file_descr : [`Peek | `Take] -> Unix.file_descr Eio.Generic.ty

  type _ Effect.t += 
    | Await_readable : Unix.file_descr -> unit Effect.t
    | Await_writable : Unix.file_descr -> unit Effect.t
    | Get_system_clock : Eio.Time.clock Effect.t
    | Socket_of_fd : Eio.Switch.t * bool * Unix.file_descr -> socket Effect.t
    | Socketpair : Eio.Switch.t * Unix.socket_domain * Unix.socket_type * int -> (socket * socket) Effect.t
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
  let peek x = x#unix_fd `Peek
  let take x = x#unix_fd `Take

  let peek_opt x = Eio.Generic.probe x (Private.Unix_file_descr `Peek)
  let take_opt x = Eio.Generic.probe x (Private.Unix_file_descr `Take)

  let as_socket ~sw ~close_unix fd = Effect.perform (Private.Socket_of_fd (sw, close_unix, fd))
end

let socketpair ~sw ?(domain=Unix.PF_UNIX) ?(ty=Unix.SOCK_STREAM) ?(protocol=0) () =
  Effect.perform (Private.Socketpair (sw, domain, ty, protocol))

module Ipaddr = struct
  let to_unix : _ Eio.Net.Ipaddr.t -> Unix.inet_addr = Obj.magic
  let of_unix : Unix.inet_addr -> _ Eio.Net.Ipaddr.t = Obj.magic
end

module Ctf = Ctf_unix

let getnameinfo (sockaddr : Eio.Net.Sockaddr.t) =
  let sockaddr, options =
    match sockaddr with
    | `Unix s -> (Unix.ADDR_UNIX s, [])
    | `Tcp (addr, port) -> (Unix.ADDR_INET (Ipaddr.to_unix addr, port), [])
    | `Udp (addr, port) -> (Unix.ADDR_INET (Ipaddr.to_unix addr, port), [Unix.NI_DGRAM])
  in
  run_in_systhread (fun () ->
    let Unix.{ni_hostname; ni_service} = Unix.getnameinfo sockaddr options in
    (ni_hostname, ni_service))
