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

(* Run an event loop in the current domain, using [fn x] as the root fiber. *)
let run_event_loop fn x =
  Sched.with_sched @@ fun sched ->
  let open Effect.Deep in
  let extra_effects : _ effect_handler = {
    effc = fun (type a) (e : a Effect.t) : ((a, Sched.exit) continuation -> Sched.exit) option ->
      match e with
      | Eio_unix.Private.Get_monotonic_clock -> Some (fun k -> continue k (Time.mono_clock : Eio.Time.Mono.t))
      | Eio_unix.Private.Socket_of_fd (sw, close_unix, unix_fd) -> Some (fun k ->
          let fd = Fd.of_unix ~sw ~blocking:false ~close_unix unix_fd in
          (* TODO: On Windows, if the FD from Unix.pipe () is passed this will fail *)
          (try Unix.set_nonblock unix_fd with Unix.Unix_error (Unix.ENOTSOCK, _, _) -> ());
          continue k (Flow.of_fd fd :> Eio_unix.socket)
        )
      | Eio_unix.Private.Socketpair (sw, domain, ty, protocol) -> Some (fun k ->
          match
            let unix_a, unix_b = Unix.socketpair ~cloexec:true domain ty protocol in
            let a = Fd.of_unix ~sw ~blocking:false ~close_unix:true unix_a in
            let b = Fd.of_unix ~sw ~blocking:false ~close_unix:true unix_b in
            Unix.set_nonblock unix_a;
            Unix.set_nonblock unix_b;
            (Flow.of_fd a :> Eio_unix.socket), (Flow.of_fd b :> Eio_unix.socket)
          with
          | r -> continue k r
          | exception Unix.Unix_error (code, name, arg) ->
              discontinue k (Err.wrap code name arg)
        )
      | Eio_unix.Private.Pipe sw -> Some (fun k ->
          match
            let r, w = Low_level.pipe ~sw in
            let source = (Flow.of_fd r :> Eio_unix.source) in
            let sink = (Flow.of_fd w :> Eio_unix.sink) in
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

let v = object
  inherit Eio.Domain_manager.t

  method run_raw fn =
    let domain = ref None in
    Eio.Private.Suspend.enter (fun _ctx enqueue ->
        domain := Some (Domain.spawn (fun () -> Fun.protect fn ~finally:(fun () -> enqueue (Ok ()))))
      );
    Domain.join (Option.get !domain)

  method run fn =
    let domain = ref None in
    Eio.Private.Suspend.enter (fun ctx enqueue ->
        let cancelled, set_cancelled = Promise.create () in
        Eio.Private.Fiber_context.set_cancel_fn ctx (Promise.resolve set_cancelled);
        domain := Some (Domain.spawn (fun () ->
            Fun.protect (run_event_loop (fun () -> fn ~cancelled))
              ~finally:(fun () -> enqueue (Ok ()))))
      );
    Domain.join (Option.get !domain)
end
