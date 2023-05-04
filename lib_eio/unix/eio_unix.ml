[@@@alert "-unstable"]

module Fd = Fd
module Resource = Resource
module Private = Private

include Types

let await_readable = Private.await_readable
let await_writable = Private.await_writable
let pipe = Private.pipe

type Eio.Exn.Backend.t += Unix_error of Unix.error * string * string
let () =
  Eio.Exn.Backend.register_pp (fun f -> function
      | Unix_error (code, name, arg) -> Fmt.pf f "Unix_error (%s, %S, %S)" (Unix.error_message code) name arg; true
      | _ -> false
    )

let sleep d =
  Eio.Time.Mono.sleep (Effect.perform Private.Get_monotonic_clock) d

let run_in_systhread fn =
  let f fiber enqueue =
    match Eio.Private.Fiber_context.get_error fiber with
    | Some err -> enqueue (Error err)
    | None ->
      let _t : Thread.t = Thread.create (fun () -> enqueue (try Ok (fn ()) with exn -> Error exn)) () in
      ()
  in
  Effect.perform (Eio.Private.Effects.Suspend f)

let import_socket_stream ~sw ~close_unix fd = Effect.perform (Private.Socket_of_fd (sw, close_unix, fd))

(* Deprecated *)
module FD = struct
  let peek t = Fd.use_exn "peek" (Resource.fd t) Fun.id

  let peek_opt t =
    match Resource.fd_opt t with
    | None -> None
    | Some fd -> Some (Fd.use_exn "peek_opt" fd Fun.id)

  let take t = Fd.remove (Resource.fd t) |> Option.get

  let take_opt t =
    match Resource.fd_opt t with
    | None -> None
    | Some fd -> Fd.remove fd

  let as_socket = import_socket_stream
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

module Process = Process

module Stdenv = struct
  type base = <
    stdin  : source;
    stdout : sink;
    stderr : sink;
    net : Eio.Net.t;
    domain_mgr : Eio.Domain_manager.t;
    process_mgr : Process.mgr;
    clock : Eio.Time.clock;
    mono_clock : Eio.Time.Mono.t;
    fs : Eio.Fs.dir Eio.Path.t;
    cwd : Eio.Fs.dir Eio.Path.t;
    secure_random : Eio.Flow.source;
    debug : Eio.Debug.t;
  >
end
