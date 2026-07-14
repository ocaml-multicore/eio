(*
 * Copyright (C) 2020-2021 Anil Madhavapeddy
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

module Fiber_context = Eio.Private.Fiber_context
module Trace = Eio.Private.Trace
module Fd = Eio_unix.Fd
module Suspended = Eio_utils.Suspended

module Low_level = Low_level

type stdenv = Eio_unix.Stdenv.base

let wrap_backtrace fn x =
  match fn x with
  | x -> Ok x
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    Error (ex, bt)

let unwrap_backtrace = function
  | Ok x -> x
  | Error (ex, bt) -> Printexc.raise_with_backtrace ex bt

module Domain_mgr = struct
  type t = {
    run_event_loop : (unit -> unit) -> unit -> unit;
  }

  let domain_spawn sched k fn =
    Domain.spawn @@ fun () ->
    Trace.domain_spawn ~parent:(Suspended.tid k);
    Fun.protect fn ~finally:(fun () -> Sched.enqueue_thread sched k ())

  let make ~run_event_loop = { run_event_loop }

  let run_raw _t fn =
    let domain = ref None in
    Sched.enter "run-domain" (fun sched k ->
        let fn = wrap_backtrace fn in
        domain := Some (domain_spawn sched k fn)
      );
    unwrap_backtrace (Domain.join (Option.get !domain))

  let run t fn =
    let domain = ref None in
    Sched.enter "run-domain" (fun sched k ->
        let cancelled, set_cancelled = Promise.create () in
        Fiber_context.set_cancel_fn k.fiber (Promise.resolve set_cancelled);
        domain := Some (domain_spawn sched k (fun () ->
            let result = ref None in
            let fn = wrap_backtrace (fun () -> fn ~cancelled) in
            t.run_event_loop (fun () -> result := Some (fn ())) ();
            Option.get !result
          ))
      );
    Trace.with_span "Domain.join" @@ fun () ->
    unwrap_backtrace (Domain.join (Option.get !domain))
end

let domain_mgr ~run_event_loop =
  let handler = Eio.Domain_manager.Pi.mgr (module Domain_mgr) in
  Eio.Resource.T (Domain_mgr.make ~run_event_loop, handler)

let stdenv ~run_event_loop =
  let fs = (Fs.dir ~label:"fs" ~path:"" Fs, "") in
  let cwd = (Fs.dir ~label:"cwd" ~path:"" Cwd, "") in
  object (_ : stdenv)
    method stdin  = Flow.stdin
    method stdout = Flow.stdout
    method stderr = Flow.stderr
    method net = Net.v
    method process_mgr = Process.mgr
    method domain_mgr = domain_mgr ~run_event_loop
    method clock = Time.clock
    method mono_clock = Time.mono_clock
    method fs = (fs :> Eio.Fs.dir_ty Eio.Path.t)
    method cwd = (cwd :> Eio.Fs.dir_ty Eio.Path.t)
    method secure_random = Flow.secure_random
    method debug = Eio.Private.Debug.v
    method backend_id = "linux"
  end

let run_event_loop (type a) ?fallback config (main : _ -> a) arg : a =
  Sched.with_sched ?fallback config @@ fun st ->
  let open Effect.Deep in
  let extra_effects : _ effect_handler = {
    effc = fun (type a) (e : a Effect.t) : ((a, Sched.exit) continuation -> Sched.exit) option ->
      match e with
      | Eio_unix.Private.Get_monotonic_clock -> Some (fun k -> continue k Time.mono_clock)
      | Eio_unix.Net.Import_socket_stream (sw, close_unix, fd) -> Some (fun k ->
          let fd = Fd.of_unix ~sw ~seekable:false ~close_unix fd in
          continue k (Flow.of_fd fd :> _ Eio_unix.Net.stream_socket)
        )
      | Eio_unix.Net.Import_socket_listening (sw, close_unix, fd) -> Some (fun k ->
          let fd = Fd.of_unix ~sw ~seekable:false ~close_unix fd in
          continue k (Net.listening_socket fd)
        )
      | Eio_unix.Net.Import_socket_datagram (sw, close_unix, fd) -> Some (fun k ->
          let fd = Fd.of_unix ~sw ~seekable:false ~close_unix fd in
          continue k (Net.datagram_socket fd)
        )
      | Eio_unix.Net.Socketpair_stream (sw, domain, protocol) -> Some (fun k ->
          match
            let a, b = Unix.socketpair ~cloexec:true domain Unix.SOCK_STREAM protocol in
            let a = Fd.of_unix ~sw ~seekable:false ~close_unix:true a |> Flow.of_fd in
            let b = Fd.of_unix ~sw ~seekable:false ~close_unix:true b |> Flow.of_fd in
            ((a :> _ Eio_unix.Net.stream_socket), (b :> _ Eio_unix.Net.stream_socket))
          with
          | r -> continue k r
          | exception Unix.Unix_error (code, name, arg) ->
              discontinue k (Err.wrap code name arg)
        )
      | Eio_unix.Net.Socketpair_datagram (sw, domain, protocol) -> Some (fun k ->
          match
            let a, b = Unix.socketpair ~cloexec:true domain Unix.SOCK_DGRAM protocol in
            let a = Fd.of_unix ~sw ~seekable:false ~close_unix:true a |> Net.datagram_socket in
            let b = Fd.of_unix ~sw ~seekable:false ~close_unix:true b |> Net.datagram_socket in
            ((a :> _ Eio_unix.Net.datagram_socket), (b :> _ Eio_unix.Net.datagram_socket))
          with
          | r -> continue k r
          | exception Unix.Unix_error (code, name, arg) ->
              discontinue k (Err.wrap code name arg)
        )
      | Eio_unix.Private.Pipe sw -> Some (fun k ->
          match
            let r, w = Low_level.pipe ~sw in
            let r = (Flow.of_fd r :> _ Eio_unix.source) in
            let w = (Flow.of_fd w :> _ Eio_unix.sink) in
            (r, w)
          with
          | r -> continue k r
          | exception Unix.Unix_error (code, name, arg) ->
            discontinue k (Err.wrap code name arg)
        )
      | _ -> None
  } in
  Sched.run ~extra_effects st main arg

let run ?queue_depth ?n_blocks ?block_size ?polling_timeout ?fallback main =
  let config = Sched.config ?queue_depth ?n_blocks ?block_size ?polling_timeout () in
  let stdenv = stdenv ~run_event_loop:(run_event_loop ?fallback:None config) in
  (* SIGPIPE makes no sense in a modern application. *)
  Sys.(set_signal sigpipe Signal_ignore);
  run_event_loop ?fallback config main stdenv
