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

(* Run an event loop in the current domain, using [fn x] as the root fiber. *)
let run_event_loop fn x =
  Sched.with_sched @@ fun sched ->
  let open Effect.Deep in
  let extra_effects : _ effect_handler = {
    effc = fun (type a) (e : a Effect.t) : ((a, Sched.exit) continuation -> Sched.exit) option ->
      match e with
      | Eio_unix.Private.Get_monotonic_clock -> Some (fun k -> continue k (Time.mono_clock : Eio.Time.Mono.t))
      | Eio_unix.Private.Socket_of_fd (sw, close_unix, fd) -> Some (fun k ->
          Unix.set_nonblock fd;
          let fd = Fd.of_unix ~sw ~blocking:false ~close_unix fd in
          continue k (Flow.of_fd fd :> Eio_unix.socket)
        )
      | Eio_unix.Private.Socketpair (sw, domain, ty, protocol) -> Some (fun k ->
          let a, b = Unix.socketpair ~cloexec:true domain ty protocol in
          Unix.set_nonblock a;
          Unix.set_nonblock b;
          let a = Fd.of_unix ~sw ~blocking:false ~close_unix:true a |> Flow.of_fd in
          let b = Fd.of_unix ~sw ~blocking:false ~close_unix:true b |> Flow.of_fd in
          continue k ((a :> Eio_unix.socket), (b :> Eio_unix.socket))
        )
      | Eio_unix.Private.Pipe sw -> Some (fun k ->
          let r, w = Unix.pipe ~cloexec:true () in
          Unix.set_nonblock r;
          Unix.set_nonblock w;
          let make x = Flow.of_fd (Fd.of_unix ~sw ~blocking:false ~close_unix:true x) in
          let r = (make r :> <Eio.Flow.source; Eio.Flow.close; Eio_unix.unix_fd>) in
          let w = (make w :> <Eio.Flow.sink; Eio.Flow.close; Eio_unix.unix_fd>) in
          continue k (r, w)
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
