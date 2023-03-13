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

module Low_level = Low_level
module Process = Process

type stdenv = <
  stdin  : <Eio.Flow.source; Eio_unix.unix_fd>;
  stdout : <Eio.Flow.sink; Eio_unix.unix_fd>;
  stderr : <Eio.Flow.sink; Eio_unix.unix_fd>;
  net : Eio.Net.t;
  domain_mgr : Eio.Domain_manager.t;
  clock : Eio.Time.clock;
  mono_clock : Eio.Time.Mono.t;
  fs : Eio.Fs.dir Eio.Path.t;
  cwd : Eio.Fs.dir Eio.Path.t;
  secure_random : Eio.Flow.source;
  debug : Eio.Debug.t;
>

let run main =
  (* SIGPIPE makes no sense in a modern application. *)
  Sys.(set_signal sigpipe Signal_ignore);
  Sys.(set_signal sigchld (Signal_handle (fun (_:int) -> Process.handle_sigchld ())));
  let stdin = (Flow.of_fd Low_level.Fd.stdin :> <Eio.Flow.source; Eio_unix.unix_fd>) in
  let stdout = (Flow.of_fd Low_level.Fd.stdout :> <Eio.Flow.sink; Eio_unix.unix_fd>) in
  let stderr = (Flow.of_fd Low_level.Fd.stderr :> <Eio.Flow.sink; Eio_unix.unix_fd>) in
  Domain_mgr.run_event_loop main @@ object (_ : stdenv)
    method stdin = stdin
    method stdout = stdout
    method stderr = stderr
    method debug = Eio.Private.Debug.v
    method clock = Time.clock
    method mono_clock = Time.mono_clock
    method net = Net.v
    method domain_mgr = Domain_mgr.v
    method cwd = ((Fs.cwd, "") :> Eio.Fs.dir Eio.Path.t)
    method fs = ((Fs.fs, "") :> Eio.Fs.dir Eio.Path.t)
    method secure_random = Flow.secure_random
  end
