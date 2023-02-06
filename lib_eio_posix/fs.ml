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

class virtual posix_dir = object
  inherit Eio.Fs.dir

  val virtual opt_nofollow : Low_level.Open_flags.t
  (** Extra flags for open operations. Sandboxes will add [O_NOFOLLOW] here. *)

  method virtual private resolve : string -> string
  (** [resolve path] returns the real path that should be used to access [path].
      For sandboxes, this is [realpath path] (and it checks that it is within the sandbox).
      For unrestricted access, this is the identity function. *)

  method virtual with_parent_dir : 'a. (string -> (Fd.t option -> string -> 'a) -> 'a)
  (** [with_parent_dir path fn] runs [fn dir_fd rel_path],
      where [rel_path] accessed relative to [dir_fd] gives access to [path].
      For unrestricted access, this just runs [fn None path].
      For sandboxes, it opens the parent of [path] as [dir_fd] and runs [fn (Some dir_fd) (basename path)]. *)
end

(* When renaming, we get a plain [Eio.Fs.dir]. We need extra access to check
   that the new location is within its sandbox. *)
type _ Eio.Generic.ty += Posix_dir : posix_dir Eio.Generic.ty
let as_posix_dir x = Eio.Generic.probe x Posix_dir

class virtual dir ~label = object (self)
  inherit posix_dir

  val mutable closed = false

  method! probe : type a. a Eio.Generic.ty -> a option = function
    | Posix_dir -> Some (self :> posix_dir)
    | _ -> None

  method open_in ~sw path =
    let fd = Err.run (Low_level.openat ~mode:0 ~sw (self#resolve path)) Low_level.Open_flags.(opt_nofollow + rdonly) in
    (Flow.of_fd fd :> <Eio.File.ro; Eio.Flow.close>)

  method open_out ~sw ~append ~create path =
    let mode, flags =
      match create with
      | `Never            -> 0,    Low_level.Open_flags.empty
      | `If_missing  perm -> perm, Low_level.Open_flags.creat
      | `Or_truncate perm -> perm, Low_level.Open_flags.(creat + trunc)
      | `Exclusive   perm -> perm, Low_level.Open_flags.(creat + excl)
    in
    let flags = if append then Low_level.Open_flags.(flags + append) else flags in
    let flags = Low_level.Open_flags.(flags + rdwr + opt_nofollow) in
    match
      self#with_parent_dir path @@ fun dirfd path ->
      Low_level.openat ?dirfd ~sw ~mode path flags
    with
    | fd -> (Flow.of_fd fd :> <Eio.File.rw; Eio.Flow.close>)
    | exception Unix.Unix_error (ELOOP, _, _) ->
      (* The leaf was a symlink (or we're unconfined and the main path changed, but ignore that).
         A leaf symlink might be OK, but we need to check it's still in the sandbox.
         todo: possibly we should limit the number of redirections here, like the kernel does. *)
      let target = Unix.readlink path in
      let full_target =
        if Filename.is_relative target then
          Filename.concat (Filename.dirname path) target
        else target
      in
      self#open_out ~sw ~append ~create full_target
    | exception Unix.Unix_error (code, name, arg) ->
      raise (Err.wrap code name arg)

  method mkdir ~perm path =
    self#with_parent_dir path @@ fun dirfd path ->
    Err.run (Low_level.mkdir ?dirfd ~mode:perm) path

  method unlink path =
    self#with_parent_dir path @@ fun dirfd path ->
    Err.run (Low_level.unlink ?dirfd ~dir:false) path

  method rmdir path =
    self#with_parent_dir path @@ fun dirfd path ->
    Err.run (Low_level.unlink ?dirfd ~dir:true) path

  method read_dir path =
    (* todo: need readdirat and with_parent_dir here to avoid races *)
    let path = self#resolve path in
    Err.run Low_level.readdir path
    |> Array.to_list

  method rename old_path new_dir new_path =
    match as_posix_dir new_dir with
    | None -> invalid_arg "Target is not an eio_posix directory!"
    | Some new_dir ->
      self#with_parent_dir old_path @@ fun old_dir old_path ->
      new_dir#with_parent_dir new_path @@ fun new_dir new_path ->
      Err.run (Low_level.rename ?old_dir old_path ?new_dir) new_path

  method open_dir ~sw path =
    Switch.check sw;
    let label = Filename.basename path in
    let d = new sandbox ~label (self#resolve path) in
    Switch.on_release sw (fun () -> d#close);
    (d :> Eio.Fs.dir_with_close)

  method close = closed <- true

  method pp f = Fmt.string f (String.escaped label)
end

and sandbox ~label dir_path = object (self)
  inherit dir ~label

  val opt_nofollow = Low_level.Open_flags.nofollow

  (* Resolve a relative path to an absolute one, with no symlinks.
     @raise Eio.Fs.Permission_denied if it's outside of [dir_path]. *)
  method private resolve path =
    if closed then Fmt.invalid_arg "Attempt to use closed directory %S" dir_path;
    if Filename.is_relative path then (
      let dir_path = Err.run Low_level.realpath dir_path in
      let full = Err.run Low_level.realpath (Filename.concat dir_path path) in
      let prefix_len = String.length dir_path + 1 in
      if String.length full >= prefix_len && String.sub full 0 prefix_len = dir_path ^ Filename.dir_sep then
        full
      else if full = dir_path then
        full
      else
        raise @@ Eio.Fs.err (Permission_denied (Err.Outside_sandbox (full, dir_path)))
    ) else (
      raise @@ Eio.Fs.err (Permission_denied Err.Absolute_path)
    )

  method with_parent_dir path fn =
    if closed then Fmt.invalid_arg "Attempt to use closed directory %S" dir_path;
    let dir, leaf = Filename.dirname path, Filename.basename path in
    if leaf = ".." then (
      (* We could be smarter here and normalise the path first, but '..'
         doesn't make sense for any of the current uses of [with_parent_dir]
         anyway. *)
      raise (Eio.Fs.err (Permission_denied (Err.Invalid_leaf leaf)))
    ) else (
      let dir = self#resolve dir in
      Switch.run @@ fun sw ->
      let dirfd = Low_level.openat ~sw ~mode:0 dir Low_level.Open_flags.(directory + rdonly + nofollow) in
      fn (Some dirfd) leaf
    )
end

(* Full access to the filesystem. *)
let fs = object
  inherit dir ~label:"fs"

  val opt_nofollow = Low_level.Open_flags.empty

  (* No checks *)
  method private resolve path = path
  method private with_parent_dir path fn = fn None path
end

let cwd = new sandbox ~label:"cwd" "."
