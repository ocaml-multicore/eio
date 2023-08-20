[@@@alert "-unstable"]

open Eio.Std

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

let run_in_systhread = Private.run_in_systhread

module Ipaddr = Net.Ipaddr

module Ctf = Ctf_unix

module Process = Process
module Net = Net
module Pi = Pi

module Stdenv = struct
  type base = <
    stdin  : source_ty r;
    stdout : sink_ty r;
    stderr : sink_ty r;
    net : [`Unix | `Generic] Eio.Net.ty r;
    domain_mgr : Eio.Domain_manager.t;
    process_mgr : Process.mgr;
    clock : float Eio.Time.clock_ty r;
    mono_clock : Eio.Time.Mono.ty r;
    fs : Eio.Fs.dir_ty Eio.Path.t;
    cwd : Eio.Fs.dir_ty Eio.Path.t;
    secure_random : Eio.Flow.source_ty r;
    debug : Eio.Debug.t;
    backend_id: string;
  >
end
