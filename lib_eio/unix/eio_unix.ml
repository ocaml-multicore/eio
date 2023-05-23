[@@@alert "-unstable"]

module Fd = Fd
module Resource = Resource
module Private = Private

include Types
type socket = Net.stream_socket

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

let run_in_systhread = Private.run_in_systhread

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

  let as_socket = Net.import_socket_stream
end

module Ipaddr = Net.Ipaddr

let socketpair ~sw ?domain ?(ty=Unix.SOCK_STREAM) ?protocol () =
  assert (ty = Unix.SOCK_STREAM);
  Net.socketpair_stream ~sw ?domain ?protocol ()

module Ctf = Ctf_unix

let getnameinfo = Net.getnameinfo

module Process = Process
module Net = Net

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
