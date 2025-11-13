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

let run_in_systhread = Thread_pool.run_in_systhread

module Ipaddr = Net.Ipaddr

module Process = Process
module Net = Net
module Cap = Cap
module Pi = Pi

module Stdenv = struct
  type base = <
    stdin  : source_ty r;
    stdout : sink_ty r;
    stderr : sink_ty r;
    net : [`Unix | `Generic] Eio.Net.ty r;
    domain_mgr : Eio.Domain_manager.ty r;
    process_mgr : Process.mgr_ty r;
    clock : float Eio.Time.clock_ty r;
    mono_clock : Eio.Time.Mono.ty r;
    fs : Eio.Fs.dir_ty Eio.Path.t;
    cwd : Eio.Fs.dir_ty Eio.Path.t;
    secure_random : Eio.Flow.source_ty r;
    debug : Eio.Debug.t;
    backend_id: string;
  >

  let with_env
    ?stdin ?stdout ?stderr ?net ?domain_mgr
    ?process_mgr ?clock ?mono_clock ?fs ?cwd
    ?secure_random ?debug ?backend_id (env : base) : base
  =
    object
      method stdin = Option.value ~default:env#stdin stdin
      method stdout = Option.value ~default:env#stdout stdout 
      method stderr = Option.value ~default:env#stderr stderr 
      method net = Option.value ~default:env#net net 
      method domain_mgr = Option.value ~default:env#domain_mgr domain_mgr 
      method process_mgr = Option.value ~default:env#process_mgr process_mgr
      method clock = Option.value ~default:env#clock clock
      method mono_clock = Option.value ~default:env#mono_clock mono_clock
      method fs = Option.value ~default:env#fs fs
      method cwd = Option.value ~default:env#cwd cwd
      method secure_random = Option.value ~default:env#secure_random secure_random 
      method debug = Option.value ~default:env#debug debug
      method backend_id = Option.value ~default:env#backend_id backend_id
    end
end
