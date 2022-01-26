open Effect

module Hook = Hook
module Cancel = Cancel

module Std = struct
  module Promise = Promise
  module Fibre = Fibre
  module Switch = Switch

  type _ eff += Trace : (?__POS__:(string * int * int * int) -> ('a, Format.formatter, unit, unit) format4 -> 'a) eff

  let traceln ?__POS__ fmt =
    perform Trace ?__POS__ fmt
end

module Semaphore = Semaphore
module Stream = Stream
module Exn = Exn
module Generic = Generic
module Flow = Flow
module Buf_read = Buf_read
module Net = Net
module Domain_manager = Domain_manager
module Time = Time
module Unix_perm = Dir.Unix_perm
module Dir = Dir

module Stdenv = struct
  type t = <
    stdin  : Flow.source;
    stdout : Flow.sink;
    stderr : Flow.sink;
    net : Net.t;
    domain_mgr : Domain_manager.t;
    clock : Time.clock;
    fs : Dir.t;
    cwd : Dir.t;
  >

  let stdin  (t : <stdin  : #Flow.source; ..>) = t#stdin
  let stdout (t : <stdout : #Flow.sink;   ..>) = t#stdout
  let stderr (t : <stderr : #Flow.sink;   ..>) = t#stderr
  let net (t : <net : #Net.t; ..>) = t#net
  let domain_mgr (t : <domain_mgr : #Domain_manager.t; ..>) = t#domain_mgr
  let clock (t : <clock : #Time.clock; ..>) = t#clock
  let fs (t : <fs : #Dir.t; ..>) = t#fs
  let cwd (t : <cwd : #Dir.t; ..>) = t#cwd
end

module Private = struct
  module Fibre_context = Cancel.Fibre_context

  module Effects = struct
    type 'a enqueue = 'a Suspend.enqueue
    type _ eff += 
      | Suspend = Suspend.Suspend
      | Fork = Fibre.Fork
      | Get_context = Cancel.Get_context
      | Trace = Std.Trace
  end

  module Effect = Effect
end
