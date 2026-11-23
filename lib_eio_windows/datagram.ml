(* Copyright (C) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: ISC *)

let send fd ?dst buf =
  Eio_unix.Fd.use_exn "send_msg" fd @@ fun raw ->
  Completion.await (fun port ~data ->
    match dst with
    | Some dst -> Iocp.Event.sendto port ~data raw buf dst
    | None -> Iocp.Event.send port ~data raw buf)

let recv fd buf =
  Eio_unix.Fd.use_exn "recv_msg" fd @@ fun raw ->
  let receive () =
    let c = Completion.await_result (fun port ~data -> Iocp.Event.recvfrom port ~data raw buf) in
    match c.error, c.source with
    | (None | Some EMSGSIZE), Some addr ->
      c.bytes_transferred, addr
    | Some code, _ -> raise (Unix.Unix_error (code, "recv_msg", ""))
    | None, None -> raise (Unix.Unix_error (ESHUTDOWN, "recv_msg", ""))
  in
  let rec loop () =
    try receive () with
    | Unix.Unix_error ((ECONNRESET | ENETRESET), _, _) as ex ->
      (* An unconnected UDP socket can receive an ICMP error for an earlier send. *)
      match Unix.getpeername raw with
      | _ -> raise ex
      | exception Unix.Unix_error (ENOTCONN, _, _) -> Eio.Fiber.yield (); loop ()
  in
  loop ()
