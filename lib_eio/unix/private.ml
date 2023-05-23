[@@@alert "-unstable"]

open Eio.Std
open Types

type _ Effect.t +=
  | Await_readable : Unix.file_descr -> unit Effect.t
  | Await_writable : Unix.file_descr -> unit Effect.t
  | Get_monotonic_clock : Eio.Time.Mono.t Effect.t
  | Pipe : Switch.t -> (source * sink) Effect.t

let await_readable fd = Effect.perform (Await_readable fd)
let await_writable fd = Effect.perform (Await_writable fd)

let pipe sw = Effect.perform (Pipe sw)

module Rcfd = Rcfd
module Fork_action = Fork_action

let run_in_systhread fn =
  let f fiber enqueue =
    match Eio.Private.Fiber_context.get_error fiber with
    | Some err -> enqueue (Error err)
    | None ->
      let _t : Thread.t = Thread.create (fun () -> enqueue (try Ok (fn ()) with exn -> Error exn)) () in
      ()
  in
  Effect.perform (Eio.Private.Effects.Suspend f)
