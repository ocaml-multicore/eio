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

(* This module provides (optional) sandboxing, allowing operations to be restricted to a subtree.

   On FreeBSD we use O_RESOLVE_BENEATH and let the OS handle everything for us.
   On other systems we resolve one path component at a time. *)

open Eio.Std

module Fd = Eio_unix.Fd

(* When renaming, we get a plain [Eio.Fs.dir]. We need extra access to check
   that the new location is within its sandbox. *)
type (_, _, _) Eio.Resource.pi += Posix_dir : ('t, 't -> Low_level.dir_fd, [> `Posix_dir]) Eio.Resource.pi

let as_posix_dir (Eio.Resource.T (t, ops)) =
  match Eio.Resource.get_opt ops Posix_dir with
  | None -> None
  | Some fn -> Some (fn t)

module rec Dir : sig
  include Eio.Fs.Pi.DIR

  val v : label:string -> path:string -> Low_level.dir_fd -> t

  val fd : t -> Low_level.dir_fd
end = struct
  type t = {
    fd : Low_level.dir_fd;
    dir_path : string;
    label : string;
  }

  let fd t = t.fd

  let v ~label ~path:dir_path fd = { fd; dir_path; label }

  let open_in t ~sw path =
    let fd = Err.run (Low_level.openat ~mode:0 ~sw t.fd path) Low_level.Open_flags.rdonly in
    (Flow.of_fd fd :> Eio.File.ro_ty Eio.Resource.t)

  let open_out t ~sw ~append ~create path =
    let mode, flags =
      match create with
      | `Never            -> 0,    Low_level.Open_flags.empty
      | `If_missing  perm -> perm, Low_level.Open_flags.creat
      | `Or_truncate perm -> perm, Low_level.Open_flags.(creat + trunc)
      | `Exclusive   perm -> perm, Low_level.Open_flags.(creat + excl)
    in
    let flags = if append then Low_level.Open_flags.(flags + append) else flags in
    let flags = Low_level.Open_flags.(flags + rdwr) in
    match Low_level.openat ~sw ~mode t.fd path flags with
    | fd -> (Flow.of_fd fd :> Eio.File.rw_ty r)
    | exception Unix.Unix_error (code, name, arg) ->
      raise (Err.wrap code name arg)

  let mkdir t ~perm path =
    Err.run (Low_level.mkdir ~mode:perm t.fd) path

  let unlink t path =
    Err.run (Low_level.unlink ~dir:false t.fd) path

  let rmdir t path =
    Err.run (Low_level.unlink ~dir:true t.fd) path

  let stat t ~follow path =
    let buf = Low_level.create_stat () in
    Err.run (Low_level.fstatat ~buf ~follow t.fd) path;
    Flow.eio_of_stat buf

  let read_dir t path =
    Err.run (Low_level.readdir t.fd) path
    |> Array.to_list

  let read_link t path =
    Err.run (Low_level.read_link t.fd) path

  let rename t old_path new_dir new_path =
    match as_posix_dir new_dir with
    | None -> invalid_arg "Target is not an eio_posix directory!"
    | Some new_dir -> Err.run (Low_level.rename t.fd old_path new_dir) new_path

  let symlink ~link_to t path =
    Err.run (Low_level.symlink ~link_to t.fd) path

  let chown ~follow ~uid ~gid t path =
    Err.run (Low_level.chown ~follow ~uid ~gid t.fd) path

  let open_dir t ~sw path =
    let flags = Low_level.Open_flags.(rdonly + directory +? path) in
    let fd = Err.run (Low_level.openat ~sw ~mode:0 t.fd path) flags in
    let label = Filename.basename path in
    let full_path = if Filename.is_relative path then Filename.concat t.dir_path path else path in
    let d = v ~label ~path:full_path (Fd fd) in
    Eio.Resource.T (d, Handler.v)

  let pp f t = Fmt.string f (String.escaped t.label)

  let native_internal t path =
    if Filename.is_relative path then (
      let p =
        if t.dir_path = "." then path
        else Filename.concat t.dir_path path
      in
      if p = "" then "."
      else if p = "." then p
      else if Filename.is_implicit p then "./" ^ p
      else p
    ) else path

  let native t path =
    Some (native_internal t path)
end
and Handler : sig
  val v : (Dir.t, [`Dir | `Close]) Eio.Resource.handler
end = struct
  let v = Eio.Resource.handler [
      H (Eio.Fs.Pi.Dir, (module Dir));
      H (Posix_dir, Dir.fd);
    ]
end

(* Full access to the filesystem. *)
let fs = Eio.Resource.T (Dir.v ~label:"fs" ~path:"." Fs, Handler.v)
let cwd = Eio.Resource.T (Dir.v ~label:"cwd" ~path:"." Cwd, Handler.v)
