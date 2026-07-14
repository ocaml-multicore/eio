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

module Impl = struct
  type t = Low_level.Process.t
  type tag = [ `Generic | `Unix ]

  let pid = Low_level.Process.pid

  let await t =
    match Eio.Promise.await @@ Low_level.Process.exit_status t with
    | Unix.WEXITED i -> `Exited i
    | Unix.WSIGNALED i -> `Signaled i
    | Unix.WSTOPPED _ -> assert false

  let signal = Low_level.Process.signal
end

let process =
  let handler = Eio.Process.Pi.process (module Impl) in
  fun proc -> Eio.Resource.T (proc, handler)

module Mgr = struct
  module T = struct
    type t = unit 
    
    let spawn_unix () ~sw ?cwd ?pgid ?uid ?gid ?login_tty ~env ~fds ~executable args =
      let login_tty_action, fds = match login_tty with
        | None -> [], fds
        | Some tty ->
          let fds = [
            0, tty, `Blocking;
            1, tty, `Blocking;
            2, tty, `Blocking;
          ] @ fds in
          [Eio_unix.Private.Fork_action.login_tty tty], fds
      in
      let actions = Low_level.Process.Fork_action.(
          login_tty_action @
          [ Eio_unix.Private.Fork_action.inherit_fds fds;
            execve executable ~argv:(Array.of_list args) ~env ]
      ) in
      let actions = match pgid with
        | None -> actions
        | Some pgid -> Eio_unix.Private.Fork_action.setpgid pgid :: actions
      in
      let actions = match uid with
        | None -> actions
        | Some uid -> Eio_unix.Private.Fork_action.setuid uid :: actions
      in
      let actions = match gid with
        | None -> actions
        | Some gid -> Eio_unix.Private.Fork_action.setgid gid :: actions
      in
      let with_actions cwd fn = match cwd with
        | None -> fn actions
        | Some (fd, s) ->
          match Fs.get_dir_fd_opt fd with
          | None -> Fmt.invalid_arg "cwd is not an OS directory!"
          | Some dir_fd ->
            Fs.with_dir dir_fd s @@ fun cwd ->
            fn (Low_level.Process.Fork_action.fchdir cwd :: actions)
      in
      with_actions cwd @@ fun actions ->
      process (Low_level.Process.spawn ~sw actions)
  end

  include Eio_unix.Process.Make_mgr (T)
end

let mgr : Eio_unix.Process.mgr_ty r =
  let h = Eio_unix.Process.Pi.mgr_unix (module Mgr) in
  Eio.Resource.T ((), h)
