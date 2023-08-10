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
        if String.length full >= prefix_len && String.sub full 0 prefix_len = dir_path ^ Filename.dir_sep then
          full
        else if full = dir_path then
          full
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
        let dirfd = Low_level.openat ~sw ~mode:0 dir Low_level.Open_flags.(directory + rdonly + nofollow) in
        fn (Some dirfd) leaf
      )
    ) else fn None path

  let v ~label ~sandbox dir_path = { dir_path; sandbox; label; closed = false }

  (* Sandboxes use [O_NOFOLLOW] when opening files ([resolve] already removed any symlinks).
     This avoids a race where symlink might be added after [realpath] returns. *)
  let opt_nofollow t =
    if t.sandbox then Low_level.Open_flags.nofollow else Low_level.Open_flags.empty

  let open_in t ~sw path =
    let fd = Err.run (Low_level.openat ~mode:0 ~sw (resolve t path)) Low_level.Open_flags.(opt_nofollow t + rdonly) in
    (Flow.of_fd fd :> Eio.File.ro_ty Eio.Resource.t)

  let rec open_out t ~sw ~append ~create path =
    let mode, flags =
      match create with
      | `Never            -> 0,    Low_level.Open_flags.empty
      | `If_missing  perm -> perm, Low_level.Open_flags.creat
      | `Or_truncate perm -> perm, Low_level.Open_flags.(creat + trunc)
      | `Exclusive   perm -> perm, Low_level.Open_flags.(creat + excl)
    in
    let flags = if append then Low_level.Open_flags.(flags + append) else flags in
    let flags = Low_level.Open_flags.(flags + rdwr + opt_nofollow t) in
    match
      with_parent_dir t path @@ fun dirfd path ->
      Low_level.openat ?dirfd ~sw ~mode path flags
    with
    | fd -> (Flow.of_fd fd :> Eio.File.rw_ty r)
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

  let stat t ~follow path k =
    let r = Low_level.create_stat () in
    if follow then (
      Err.run (Low_level.fstatat ~buf:r ~follow:true) (resolve t path);
    ) else (
      with_parent_dir t path @@ fun dirfd path ->
      Err.run (Low_level.fstatat ~buf:r ?dirfd ~follow:false) path;
    );
    let open Eio.File in
    let rec fn : type a b. (a, b) stats -> a -> b = fun v acc ->
      match v with
      | Dev :: tl -> fn tl @@ acc (Low_level.dev r)
      | Ino :: tl -> fn tl @@ acc (Low_level.ino r)
      | Kind :: tl -> fn tl @@ acc (Low_level.kind r)
      | Perm :: tl -> fn tl @@ acc (Low_level.perm r)
      | Nlink :: tl -> fn tl @@ acc (Low_level.nlink r)
      | Uid :: tl -> fn tl @@ acc (Low_level.uid r)
      | Gid :: tl -> fn tl @@ acc (Low_level.gid r)
      | Rdev :: tl -> fn tl @@ acc (Low_level.rdev r)
      | Size :: tl -> fn tl @@ acc (Low_level.size r)
      | Atime :: tl -> fn tl @@ acc (Flow.float_of_time (Low_level.atime_sec r) (Low_level.atime_nsec r))
      | Mtime :: tl -> fn tl @@ acc (Flow.float_of_time (Low_level.mtime_sec r) (Low_level.mtime_nsec r))
      | Ctime :: tl -> fn tl @@ acc (Flow.float_of_time (Low_level.ctime_sec r) (Low_level.ctime_nsec r))
      | [] -> acc
    in fn k

  let read_dir t path =
    (* todo: need fdopendir here to avoid races *)
    let path = resolve t path in
    Err.run Low_level.readdir path
    |> Array.to_list

  let rename t old_path new_dir new_path =
    match Handler.as_posix_dir new_dir with
    | None -> invalid_arg "Target is not an eio_posix directory!"
    | Some new_dir ->
      with_parent_dir t old_path @@ fun old_dir old_path ->
      with_parent_dir new_dir new_path @@ fun new_dir new_path ->
      Err.run (Low_level.rename ?old_dir old_path ?new_dir) new_path

  let close t = t.closed <- true

  let open_dir t ~sw path =
    Switch.check sw;
    let label = Filename.basename path in
    let d = v ~label (resolve t path) ~sandbox:true in
    Switch.on_release sw (fun () -> close d);
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
