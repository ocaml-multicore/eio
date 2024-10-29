include Eio__core

module Debug = Private.Debug
let traceln = Debug.traceln

module Std = Std
module Semaphore = Semaphore
module Mutex = Eio_mutex
module Condition = Condition
module Stream = Stream
module Lazy = Lazy
module Pool = Pool
module Executor_pool = Executor_pool
module Exn = Exn
module Resource = Resource
module Buf_read = Buf_read
module Flow = struct
  include Flow

  let read_all flow =
    Buf_read.(parse_exn take_all) flow ~max_size:max_int
end
module Buf_write = Buf_write
module Net = Net
module Process = Process
module Domain_manager = Domain_manager
module Time = Time
module File = File
module Fs = Fs
module Path = Path

module Stdenv = struct
  let stdin  (t : <stdin  : _ Flow.source; ..>) = t#stdin
  let stdout (t : <stdout : _ Flow.sink;   ..>) = t#stdout
  let stderr (t : <stderr : _ Flow.sink;   ..>) = t#stderr
  let net (t : <net : _ Net.t; ..>) = t#net
  let process_mgr (t : <process_mgr : _ Process.mgr; ..>) = t#process_mgr
  let domain_mgr (t : <domain_mgr : _ Domain_manager.t; ..>) = t#domain_mgr
  let clock (t : <clock : _ Time.clock; ..>) = t#clock
  let mono_clock (t : <mono_clock : _ Time.Mono.t; ..>) = t#mono_clock
  let secure_random (t: <secure_random : _ Flow.source; ..>) = t#secure_random
  let fs (t : <fs : _ Path.t; ..>) = t#fs
  let cwd (t : <cwd : _ Path.t; ..>) = t#cwd
  let debug (t : <debug : 'a; ..>) = t#debug
  let backend_id (t: <backend_id : string; ..>) = t#backend_id
end

exception Io = Exn.Io
