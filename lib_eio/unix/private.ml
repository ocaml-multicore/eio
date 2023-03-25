[@@@alert "-unstable"]

open Eio.Std
open Types

type _ Effect.t +=
  | Await_readable : Unix.file_descr -> unit Effect.t
  | Await_writable : Unix.file_descr -> unit Effect.t
  | Get_monotonic_clock : Eio.Time.Mono.t Effect.t
  | Socket_of_fd : Switch.t * bool * Unix.file_descr -> socket Effect.t
  | Socketpair : Switch.t * Unix.socket_domain * Unix.socket_type * int -> (socket * socket) Effect.t
  | Pipe : Switch.t -> (source * sink) Effect.t

let await_readable fd = Effect.perform (Await_readable fd)
let await_writable fd = Effect.perform (Await_writable fd)

let pipe sw = Effect.perform (Pipe sw)

module Rcfd = Rcfd
module Fork_action = Fork_action
