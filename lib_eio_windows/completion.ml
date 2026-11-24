(* Copyright (C) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: ISC *)

type t = Iocp.completion Eio.Promise.u Iocp.t

type _ Effect.t += Get : t Effect.t
let current () = Effect.perform Get

let dispatch = function
  | Iocp.Completion (resolver, c) -> Eio.Promise.resolve resolver c
  | Iocp.Wake -> ()

let with_port fn =
  let port = Iocp.create ~skip_on_success:true () in
  Fun.protect ~finally:(fun () -> Iocp.close port) (fun () -> fn port)
