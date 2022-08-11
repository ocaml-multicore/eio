include Eio__core

module Fibre = Fiber

module Debug = Private.Debug
let traceln = Debug.traceln

module Std = struct
  module Promise = Promise
  module Fiber = Fiber
  module Fibre = Fiber
  module Switch = Switch
  let traceln = Debug.traceln
end

module Semaphore = Semaphore
module Mutex = Eio_mutex
module Condition = Condition
module Stream = Stream
module Exn = Exn
module Generic = Generic
module Flow = Flow
module Buf_read = Buf_read
module Buf_write = Buf_write
module Net = Net
module Domain_manager = Domain_manager
module Time = Time
module Fs = Fs
module Path = Path

module Stdenv = struct
  type t = <
    stdin  : Flow.source;
    stdout : Flow.sink;
    stderr : Flow.sink;
    net : Net.t;
    domain_mgr : Domain_manager.t;
    clock : Time.clock;
    fs : Fs.dir Path.t;
    cwd : Fs.dir Path.t;
    secure_random : Flow.source;
    debug : Debug.t;
  >

  let stdin  (t : <stdin  : #Flow.source; ..>) = t#stdin
  let stdout (t : <stdout : #Flow.sink;   ..>) = t#stdout
  let stderr (t : <stderr : #Flow.sink;   ..>) = t#stderr
  let net (t : <net : #Net.t; ..>) = t#net
  let domain_mgr (t : <domain_mgr : #Domain_manager.t; ..>) = t#domain_mgr
  let clock (t : <clock : #Time.clock; ..>) = t#clock
  let secure_random (t: <secure_random : #Flow.source; ..>) = t#secure_random
  let fs (t : <fs : #Fs.dir Path.t; ..>) = t#fs
  let cwd (t : <cwd : #Fs.dir Path.t; ..>) = t#cwd
  let debug (t : <debug : 'a; ..>) = t#debug
end
