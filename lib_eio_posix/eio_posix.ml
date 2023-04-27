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

type stdenv = Eio_unix.Stdenv.base

let run main =
  (* SIGPIPE makes no sense in a modern application. *)
  Sys.(set_signal sigpipe Signal_ignore);
  Sys.(set_signal sigchld (Signal_handle (fun (_:int) -> Children.handle_sigchld ())));
  let stdin = (Flow.of_fd Eio_unix.Fd.stdin :> Eio_unix.source) in
  let stdout = (Flow.of_fd Eio_unix.Fd.stdout :> Eio_unix.sink) in
  let stderr = (Flow.of_fd Eio_unix.Fd.stderr :> Eio_unix.sink) in
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
