(*
 * Copyright (C) 2023 Thomas Leonard
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Eio.Std

[@@@alert "-unstable"]

module Fd = Eio_unix.Fd

let socketpair k ~sw ~domain ~ty ~protocol wrap_a wrap_b =
  let open Effect.Deep in
  match
    let unix_a, unix_b = Unix.socketpair ~cloexec:true domain ty protocol in
    let a = Fd.of_unix ~sw ~blocking:false ~close_unix:true unix_a in
    let b = Fd.of_unix ~sw ~blocking:false ~close_unix:true unix_b in
    Unix.set_nonblock unix_a;
    Unix.set_nonblock unix_b;
    (wrap_a a, wrap_b b)
  with
  | r -> continue k r
  | exception Unix.Unix_error (code, name, arg) ->
    discontinue k (Err.wrap code name arg)

(* Run an event loop in the current domain, using [fn x] as the root fiber. *)
let run_event_loop fn x =
  Sched.with_sched @@ fun sched ->
  let open Effect.Deep in
  let extra_effects : _ effect_handler = {
    effc = fun (type a) (e : a Effect.t) : ((a, Sched.exit) continuation -> Sched.exit) option ->
      match e with
      | Eio_unix.Private.Get_monotonic_clock -> Some (fun k -> continue k Time.mono_clock)
      | Eio_unix.Net.Import_socket_stream (sw, close_unix, unix_fd) -> Some (fun k ->
          let fd = Fd.of_unix ~sw ~blocking:false ~close_unix unix_fd in
          (* TODO: On Windows, if the FD from Unix.pipe () is passed this will fail *)
          (try Unix.set_nonblock unix_fd with Unix.Unix_error (Unix.ENOTSOCK, _, _) -> ());
          continue k (Flow.of_fd fd :> _ Eio_unix.Net.stream_socket)
        )
      | Eio_unix.Net.Import_socket_datagram (sw, close_unix, unix_fd) -> Some (fun k ->
          let fd = Fd.of_unix ~sw ~blocking:false ~close_unix unix_fd in
          Unix.set_nonblock unix_fd;
          continue k (Net.datagram_socket fd)
        )
      | Eio_unix.Net.Socketpair_stream (sw, domain, protocol) -> Some (fun k ->
          let wrap fd = (Flow.of_fd fd :> _ Eio_unix.Net.stream_socket) in
          socketpair k ~sw ~domain ~protocol ~ty:Unix.SOCK_STREAM wrap wrap
        )
      | Eio_unix.Net.Socketpair_datagram (sw, domain, protocol) -> Some (fun k ->
          let wrap fd = Net.datagram_socket fd in
          socketpair k ~sw ~domain ~protocol ~ty:Unix.SOCK_DGRAM wrap wrap
        )
      | Eio_unix.Private.Pipe sw -> Some (fun k ->
          match
            let r, w = Low_level.pipe ~sw in
            let source = Flow.of_fd r in
            let sink = Flow.of_fd w in
            (source, sink)
          with
          | r -> continue k r
          | exception Unix.Unix_error (code, name, arg) ->
            discontinue k (Err.wrap code name arg)
        )
      | _ -> None
  }
  in
  Sched.run ~extra_effects sched fn x

let wrap_backtrace fn x =
  match fn x with
  | x -> Ok x
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    Error (ex, bt)

let unwrap_backtrace = function
  | Ok x -> x
  | Error (ex, bt) -> Printexc.raise_with_backtrace ex bt

let v = object
  inherit Eio.Domain_manager.t

  method run_raw fn =
    let domain = ref None in
    Eio.Private.Suspend.enter (fun _ctx enqueue ->
        domain := Some (Domain.spawn (fun () -> Fun.protect (wrap_backtrace fn) ~finally:(fun () -> enqueue (Ok ()))))
      );
    unwrap_backtrace (Domain.join (Option.get !domain))

  method run fn =
    let domain = ref None in
    Eio.Private.Suspend.enter (fun ctx enqueue ->
        let cancelled, set_cancelled = Promise.create () in
        Eio.Private.Fiber_context.set_cancel_fn ctx (Promise.resolve set_cancelled);
        domain := Some (Domain.spawn (fun () ->
            Fun.protect (run_event_loop (wrap_backtrace (fun () -> fn ~cancelled)))
              ~finally:(fun () -> enqueue (Ok ()))))
      );
    unwrap_backtrace (Domain.join (Option.get !domain))
end
