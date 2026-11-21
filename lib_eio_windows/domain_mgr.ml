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

module Fd = Eio_unix.Fd

let handle fn k =
  match fn () with
  | x -> Effect.Deep.continue k x
  | exception ex -> Effect.Deep.discontinue_with_backtrace k ex (Printexc.get_raw_backtrace ())

let handle_io fn = handle (Err.run fn)

let import_socket ~sw ~close_unix raw =
  let fd = Fd.of_unix ~sw ~blocking:false ~close_unix raw in
  match Fd.use_exn "import_socket" fd Unix.set_nonblock with
  | () -> fd
  | exception ex -> Fd.close fd; raise ex

let socketpair ~sw ~domain ~protocol ty wrap () =
  Switch.check sw;
  let a, b = Unix.socketpair ~cloexec:true domain ty protocol in
  let import = import_socket ~sw ~close_unix:true in
  let a = try import a with ex -> Unix.close b; raise ex in
  let b = try import b with ex -> Fd.close a; raise ex in
  wrap a, wrap b

(* Run an event loop in the current domain, using [fn x] as the root fiber. *)
let run_event_loop fn x =
  let open Effect.Deep in
  let pipe_wait direction fd () =
    Readiness.Pipe.await ~sleep:(Eio.Time.Mono.sleep Time.mono_clock) direction fd
  in
  let extra_effects : _ effect_handler = {
    effc = fun (type a) (e : a Effect.t) : ((a, unit) continuation -> unit) option ->
      match e with
      | Eio_unix.Private.Await_readable fd when Readiness.Pipe.is_pipe fd -> Some (handle (pipe_wait `R fd))
      | Eio_unix.Private.Await_writable fd when Readiness.Pipe.is_pipe fd -> Some (handle (pipe_wait `W fd))
      | Eio_unix.Private.Get_monotonic_clock -> Some (fun k -> continue k Time.mono_clock)
      | Eio_unix.Private.Import_file fd -> Some (fun k ->
          continue k (Flow.of_fd fd :> Eio_unix.File.rw_ty r))
      | Eio_unix.Net.Import_socket_stream (sw, close_unix, raw) -> Some (handle (fun () ->
          let fd =
            if Readiness.is_socket raw then import_socket ~sw ~close_unix raw
            else Fd.of_unix ~sw ~blocking:true ~close_unix raw
          in
          (Flow.stream fd :> _ Eio_unix.Net.stream_socket)))
      | Eio_unix.Net.Import_socket_listening (sw, close_unix, raw) -> Some (handle (fun () ->
          Net.listening_socket ~hook:Switch.null_hook (import_socket ~sw ~close_unix raw)))
      | Eio_unix.Net.Import_socket_datagram (sw, close_unix, raw) -> Some (handle (fun () ->
          Net.datagram_socket (import_socket ~sw ~close_unix raw)))
      | Eio_unix.Net.Socketpair_stream (sw, domain, protocol) ->
        let wrap fd = (Flow.stream fd :> _ Eio_unix.Net.stream_socket) in
        Some (handle_io (socketpair ~sw ~domain ~protocol Unix.SOCK_STREAM wrap))
      | Eio_unix.Net.Socketpair_datagram (sw, domain, protocol) ->
        Some (handle_io (socketpair ~sw ~domain ~protocol Unix.SOCK_DGRAM Net.datagram_socket))
      | Eio_unix.Private.Pipe sw -> Some (handle_io (fun () ->
          let r, w = Low_level.pipe ~sw in
          Flow.of_fd ~kind:File_io.Pipe r, Flow.of_fd ~kind:File_io.Pipe w))
      | _ -> None
  }
  in
  Sched.run ~extra_effects fn x

let wrap_backtrace fn x =
  match fn x with
  | x -> Ok x
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    Error (ex, bt)

let unwrap_backtrace = function
  | Ok x -> x
  | Error (ex, bt) -> Printexc.raise_with_backtrace ex bt

module Impl = struct
  type t = unit

  let run_raw () fn =
    let domain = ref None in
    Eio.Private.Suspend.enter "run-domain" (fun _ctx enqueue ->
        domain := Some (Domain.spawn (fun () -> Fun.protect (wrap_backtrace fn) ~finally:(fun () -> enqueue (Ok ()))))
      );
    unwrap_backtrace (Domain.join (Option.get !domain))

  let run () fn =
    let domain = ref None in
    Eio.Private.Suspend.enter "run-domain" (fun ctx enqueue ->
        let cancelled, set_cancelled = Promise.create () in
        Eio.Private.Fiber_context.set_cancel_fn ctx (Promise.resolve set_cancelled);
        domain := Some (Domain.spawn (fun () ->
            Fun.protect (run_event_loop (wrap_backtrace (fun () -> fn ~cancelled)))
              ~finally:(fun () -> enqueue (Ok ()))))
      );
    unwrap_backtrace (Domain.join (Option.get !domain))
end

let v =
  let handler = Eio.Domain_manager.Pi.mgr (module Impl) in
  Eio.Resource.T ((), handler)
