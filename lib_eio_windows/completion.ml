(* Copyright (C) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: ISC *)

type t = Iocp.completion Eio.Promise.u Iocp.t

type _ Effect.t += Get : t Effect.t
let current () = Effect.perform Get

let dispatch = function
  | Iocp.Completion (resolver, c) -> Eio.Promise.resolve resolver c
  | Iocp.Wake -> ()

let ports : t list ref = ref []
let ports_lock = Mutex.create ()

let with_port fn =
  let port = Iocp.create ~skip_on_success:true () in
  Fun.protect ~finally:(fun () ->
      Mutex.protect ports_lock (fun () -> ports := List.filter ((!=) port) !ports);
      Iocp.close port)
    (fun () ->
       Mutex.protect ports_lock (fun () -> ports := port :: !ports);
       fn port)

(** Submit an operation and wait for its completion. Cancellation drains the
    operation before releasing its resources. Call [on_complete] after draining,
    including on cancellation, before propagating any error. The caller must
    keep the handle open and the buffer valid until this function returns. *)
let await_result ?(on_complete=ignore) submit =
  Eio.Fiber.check ();
  let result, resolver = Eio.Promise.create () in
  let port = current () in
  let request = submit port ~data:resolver in
  Option.iter (Eio.Promise.resolve resolver) (Iocp.result request);
  match (if Eio.Promise.is_resolved result then Eio.Fiber.yield (); Eio.Promise.await result) with
  | c -> on_complete c; c
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Cancel.protect (fun () ->
        let ex, bt =
          try Iocp.cancel port request; ex, bt
          with ex -> ex, Printexc.get_raw_backtrace ()
        in
        let c = Eio.Promise.await result in
        on_complete c;
        (* A successful or truncated datagram can have transferred zero bytes. *)
        if c.error = None || c.bytes_transferred > 0 || Option.is_some c.source then c
        else Printexc.raise_with_backtrace ex bt)

let transferred (c : Iocp.completion) =
  match c.error with
  | Some code when c.bytes_transferred = 0 -> raise (Unix.Unix_error (code, "iocp", ""))
  | _ -> c.bytes_transferred

let await ?on_complete submit = transferred (await_result ?on_complete submit)

let shutdown raw cmd =
  Iocp.Event.shutdown raw cmd;
  if cmd <> Unix.SHUTDOWN_SEND then (
    let active = Mutex.protect ports_lock (fun () -> !ports) in
    List.iter (fun port -> Iocp.Event.cancel_receives port raw) active
  )
