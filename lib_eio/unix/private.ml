[@@@alert "-unstable"]

open Eio.Std
open Types

type _ Effect.t +=
  | Await_readable : Unix.file_descr -> unit Effect.t
  | Await_writable : Unix.file_descr -> unit Effect.t
  | Get_monotonic_clock : Eio.Time.Mono.ty r Effect.t
  | Pipe : Switch.t -> (source_ty r * sink_ty r) Effect.t

let await_readable fd = Effect.perform (Await_readable fd)
let await_writable fd = Effect.perform (Await_writable fd)

let pipe sw = Effect.perform (Pipe sw)

module Rcfd = Rcfd
module Fork_action = Fork_action

let run_in_systhread ?(label="systhread") fn =
  Eio.Private.Suspend.enter label @@ fun _ctx enqueue ->
  Thread_pool.run_on_systhread ~enqueue fn

external eio_readlinkat : Unix.file_descr -> string -> Cstruct.t -> int = "eio_unix_readlinkat"

let read_link fd path =
  match fd with
  | None -> Unix.readlink path
  | Some fd ->
    Fd.use_exn "readlink" fd @@ fun fd ->
    let rec aux size =
      let buf = Cstruct.create_unsafe size in
      let len = eio_readlinkat fd path buf in
      if len < size then Cstruct.to_string ~len buf
      else aux (size * 4)
    in
    aux 1024
