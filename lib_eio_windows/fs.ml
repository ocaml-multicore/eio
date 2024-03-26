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

   For now, sandboxed directories use realpath and [O_NOFOLLOW], which is probably quite slow,
   and requires duplicating a load of path lookup logic from the kernel.
   It might be better to hold a directory FD rather than a path.
   On FreeBSD we could use O_RESOLVE_BENEATH and let the OS handle everything for us.
   On other systems we would have to resolve one path component at a time. *)

open Eio.Std

module Fd = Eio_unix.Fd

module rec Dir : sig
  include Eio.Fs.Pi.DIR

  val v : label:string -> sandbox:bool -> string -> t

  val resolve : t -> string -> string
  (** [resolve t path] returns the real path that should be used to access [path].
      For sandboxes, this is [realpath path] (and it checks that it is within the sandbox).
      For unrestricted access, this returns [path] unchanged.
      @raise Eio.Fs.Permission_denied if sandboxed and [path] is outside of [dir_path]. *)

  val with_parent_dir : t -> string -> (Fd.t option -> string -> 'a) -> 'a
  (** [with_parent_dir t path fn] runs [fn dir_fd rel_path],
      where [rel_path] accessed relative to [dir_fd] gives access to [path].
      For unrestricted access, this just runs [fn None path].
      For sandboxes, it opens the parent of [path] as [dir_fd] and runs [fn (Some dir_fd) (basename path)]. *)
end = struct
  type t = {
    dir_path : string;
    sandbox : bool;
    label : string;
    mutable closed : bool;
  }

  let resolve t path =
    if t.sandbox then (
      if t.closed then Fmt.invalid_arg "Attempt to use closed directory %S" t.dir_path;
      if Filename.is_relative path then (
        let dir_path = Err.run Low_level.realpath t.dir_path in
        let full = Err.run Low_level.realpath (Filename.concat dir_path path) in
        let prefix_len = String.length dir_path + 1 in
        (* \\??\\ Is necessary with NtCreateFile. *)
        if String.length full >= prefix_len && String.sub full 0 prefix_len = dir_path ^ Filename.dir_sep then begin
          "\\??\\" ^ full
        end else if full = dir_path then
          "\\??\\" ^ full
        else
          raise @@ Eio.Fs.err (Permission_denied (Err.Outside_sandbox (full, dir_path)))
      ) else (
        raise @@ Eio.Fs.err (Permission_denied Err.Absolute_path)
      )
    ) else path

  let with_parent_dir t path fn =
    if t.sandbox then (
      if t.closed then Fmt.invalid_arg "Attempt to use closed directory %S" t.dir_path;
      let dir, leaf = Filename.dirname path, Filename.basename path in
      if leaf = ".." then (
        (* We could be smarter here and normalise the path first, but '..'
           doesn't make sense for any of the current uses of [with_parent_dir]
           anyway. *)
        raise (Eio.Fs.err (Permission_denied (Err.Invalid_leaf leaf)))
      ) else (
        let dir = resolve t dir in
        Switch.run @@ fun sw ->
        let open Low_level in
        let dirfd = Low_level.openat ~sw ~nofollow:true dir Flags.Open.(generic_read + synchronise) Flags.Disposition.(open_if) Flags.Create.(directory) in
        fn (Some dirfd) leaf
      )
    ) else fn None path

  let v ~label ~sandbox dir_path = { dir_path; sandbox; label; closed = false }

  (* Sandboxes use [O_NOFOLLOW] when opening files ([resolve] already removed any symlinks).
     This avoids a race where symlink might be added after [realpath] returns.
     TODO: Emulate [O_NOFOLLOW] here. *)
  let opt_nofollow t = t.sandbox

  let open_in t ~sw path =
    let open Low_level in
    let fd = Err.run (Low_level.openat ~sw ~nofollow:(opt_nofollow t) (resolve t path)) Low_level.Flags.Open.(generic_read + synchronise) Flags.Disposition.(open_if) Flags.Create.(non_directory) in
    (Flow.of_fd fd :> Eio.File.ro_ty Eio.Resource.t)

  let rec open_out t ~sw ~append ~create path =
    let open Low_level in
    let _mode, disp =
      match create with
      | `Never            -> 0,    Low_level.Flags.Disposition.open_
      | `If_missing  perm -> perm, Low_level.Flags.Disposition.open_if
      | `Or_truncate perm -> perm, Low_level.Flags.Disposition.overwrite_if
      | `Exclusive   perm -> perm, Low_level.Flags.Disposition.create
    in
    let flags =
      if append then Low_level.Flags.Open.(synchronise + append)
      else Low_level.Flags.Open.(generic_write + synchronise)
    in
    match
      with_parent_dir t path @@ fun dirfd path ->
      Low_level.openat ?dirfd ~nofollow:(opt_nofollow t) ~sw path flags disp Flags.Create.(non_directory)
    with
    | fd -> (Flow.of_fd fd :> Eio.File.rw_ty r)
    (* This is the result of raising [caml_unix_error(ELOOP,...)] *)
    | exception Unix.Unix_error (EUNKNOWNERR 114, _, _) ->
      print_endline "UNKNOWN";
      (* The leaf was a symlink (or we're unconfined and the main path changed, but ignore that).
         A leaf symlink might be OK, but we need to check it's still in the sandbox.
         todo: possibly we should limit the number of redirections here, like the kernel does. *)
      let target = Unix.readlink path in
      let full_target =
        if Filename.is_relative target then
          Filename.concat (Filename.dirname path) target
        else target
      in
      open_out t ~sw ~append ~create full_target
    | exception Unix.Unix_error (code, name, arg) ->
      raise (Err.wrap code name arg)

  let mkdir t ~perm path =
    with_parent_dir t path @@ fun dirfd path ->
    Err.run (Low_level.mkdir ?dirfd ~mode:perm) path

  let unlink t path =
    with_parent_dir t path @@ fun dirfd path ->
    Err.run (Low_level.unlink ?dirfd ~dir:false) path

  let rmdir t path =
    with_parent_dir t path @@ fun dirfd path ->
    Err.run (Low_level.unlink ?dirfd ~dir:true) path

  let stat t ~follow path =
    Switch.run @@ fun sw ->
    let open Low_level in
    let flags = Low_level.Flags.Open.(generic_read + synchronise) in
    let dis = Flags.Disposition.open_if in
    let create = Flags.Create.non_directory in
    let fd = Err.run (openat ~sw ~nofollow:(not follow) (resolve t path) flags dis) create in
    Flow.Impl.stat fd

  let read_dir t path =
    (* todo: need fdopendir here to avoid races *)
    let path = resolve t path in
    Err.run Low_level.readdir path
    |> Array.to_list

  let read_link t path =
    with_parent_dir t path @@ fun dirfd path ->
    Err.run (Low_level.read_link ?dirfd) path

  let rename t old_path new_dir new_path =
    match Handler.as_posix_dir new_dir with
    | None -> invalid_arg "Target is not an eio_windows directory!"
    | Some new_dir ->
      with_parent_dir t old_path @@ fun old_dir old_path ->
      with_parent_dir new_dir new_path @@ fun new_dir new_path ->
      Err.run (Low_level.rename ?old_dir old_path ?new_dir) new_path

  let symlink t old_path new_path =
    with_parent_dir t new_path @@ fun dirfd path ->
    Err.run (Low_level.symlink old_path dirfd) path

  let close t = t.closed <- true

  let open_dir t ~sw path =
    Switch.check sw;
    let label = Filename.basename path in
    let d = v ~label (resolve t path) ~sandbox:true in
    Switch.on_release sw (fun () -> close d);
    Eio.Resource.T (d, Handler.v)

  let pp f t = Fmt.string f (String.escaped t.label)

  let native _t _path =
    failwith "TODO: Windows native"
end
and Handler : sig
  val v : (Dir.t, [`Dir | `Close]) Eio.Resource.handler

  val as_posix_dir : [> `Dir] r -> Dir.t option
end = struct
  (* When renaming, we get a plain [Eio.Fs.dir]. We need extra access to check
     that the new location is within its sandbox. *)
  type (_, _, _) Eio.Resource.pi += Posix_dir : ('t, 't -> Dir.t, [> `Posix_dir]) Eio.Resource.pi

  let as_posix_dir (Eio.Resource.T (t, ops)) =
    match Eio.Resource.get_opt ops Posix_dir with
    | None -> None
    | Some fn -> Some (fn t)

  let v = Eio.Resource.handler [
      H (Eio.Fs.Pi.Dir, (module Dir));
      H (Posix_dir, Fun.id);
    ]
end

(* Full access to the filesystem. *)
let fs = Eio.Resource.T (Dir.v ~label:"fs" ~sandbox:false ".", Handler.v)
let cwd = Eio.Resource.T (Dir.v ~label:"cwd" ~sandbox:true ".", Handler.v)
