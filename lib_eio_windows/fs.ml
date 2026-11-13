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
module Nt_path = Eio_utils.Nt_path

module rec Dir : sig
  include Eio.Fs.Pi.DIR
  val v : label:string -> sandbox:bool -> string -> t
  val resolve : t -> string -> string
  val with_parent_dir : t -> string -> (Fd.t option -> string -> 'a) -> 'a
end = struct
  type t = {
    dir_path : string;
    sandbox : bool;
    label : string;
    root : Fd.t option;
    mutable closed : bool;
  }

  let max_follows = 40  (* As Linux *)

  let checked t path = function
    | Some p -> p
    | None -> raise @@ Eio.Fs.err (Permission_denied (Err.Outside_sandbox (path, t.dir_path)))

  let relative t path =
    if not (Nt_path.is_relative path) then raise @@ Eio.Fs.err (Permission_denied Err.Absolute_path);
    checked t path (Nt_path.relative path)

  (* Opened subtrees retain their authority as a handle. Reopening [dir_path]
     would follow a junction installed on the sandbox itself. *)
  let with_root t fn =
    if t.closed then Fmt.invalid_arg "Attempt to use closed directory %S" t.dir_path;
    match t.root with
    | Some root -> fn root
    | None ->
      Switch.run @@ fun sw ->
      let open Low_level in
      (* [cwd] tracks chdir, but acquiring it must not follow a junction
         installed on it or one of its ancestors. *)
      let root = openat ~sw ~beneath:true t.dir_path Flags.Open.(generic_read + synchronise) Flags.Disposition.open_ Flags.Create.directory in
      fn root

  let open_at root ~sw ~follow path =
    Low_level.openat ~sw ~dirfd:root ~beneath:true ~follow path

  let link_target root path =
    Switch.run @@ fun sw ->
    let open Low_level in
    reparse_target (open_at root ~sw ~follow:Open_link path Flags.Open.synchronise Flags.Disposition.open_ Flags.Create.empty)

  (* Walking up from the leaf, the first prefix that opens has no link above it.
     Skip the leaf when opening the link itself. *)
  let find_link root ~follow path =
    let rec go p rest =
      match link_target root p with
      | Some target -> Some (p, target, rest)
      | None -> None
      | exception Unix.Unix_error _ -> up p rest
    and up p rest =
      match Nt_path.split p with
      | Some (parent, leaf) -> go parent (if rest = "" then leaf else Nt_path.join leaf rest)
      | None -> None
    in
    if follow = Low_level.Open_link then up path "" else go path ""

  (* Refuse kernel link traversal; resolve each link within the root and retry. *)
  let open_beneath t ~sw ~follow path flags disp create =
    with_root t @@ fun root ->
    let root_path = lazy (Low_level.realpath_fd root) in
    let rec go follows path =
      match open_at root ~sw ~follow path flags disp create with
      | fd -> fd
      | exception (Unix.Unix_error _ as ex) ->
        (* A file/directory mismatch can mask ELOOP at the leaf. *)
        match find_link root ~follow path with
        | Some (link, _, _) when link = path && disp = Low_level.Flags.Disposition.create ->
          raise (Unix.Unix_error (EEXIST, "openat", path))
        | Some (link, _, _) when link = path && follow = Low_level.Nofollow ->
          raise (Unix.Unix_error (ELOOP, "openat", path))
        | Some (link, target, rest) when follows > 0 ->
          let target = checked t link (
              Nt_path.resolve_link ~root:(Lazy.force root_path) ~parent:(Nt_path.dirname link) target
            ) in
          go (follows - 1) (if rest = "" then target else Nt_path.join target rest)
        | _ -> raise ex
    in
    go max_follows path

  let parent_and_leaf t path =
    match Nt_path.split (relative t path) with
    | Some parts -> parts
    | None -> raise (Eio.Fs.err (Permission_denied (Err.Invalid_leaf path)))

  let open_path t ~sw ~follow path flags disp create =
    Err.run (fun () ->
        if t.sandbox then open_beneath t ~sw ~follow (relative t path) flags disp create
        else Low_level.openat ~sw ~follow path flags disp create
      ) ()

  let open_dir_fd t ~sw path =
    let open Low_level in
    open_path t ~sw ~follow:Follow path Flags.Open.(generic_read + synchronise) Flags.Disposition.open_ Flags.Create.directory

  let resolve t path =
    if t.sandbox then (
      Switch.run @@ fun sw ->
      let open Low_level in
      let fd = open_path t ~sw ~follow:Follow path Flags.Open.synchronise Flags.Disposition.open_ Flags.Create.empty in
      Err.run realpath_fd fd
    )
    else path

  let with_parent_dir t path fn =
    if t.sandbox then (
      let parent, leaf = parent_and_leaf t path in
      Switch.run @@ fun sw ->
      let dirfd = open_dir_fd t ~sw parent in
      fn (Some dirfd) leaf
    ) else fn None path

  let v ~label ~sandbox dir_path = { dir_path; sandbox; label; root = None; closed = false }

  let open_in t ~sw ~follow path =
    let fd =
      open_path t ~sw path
        ~follow:(if follow then Low_level.Follow else Nofollow)
        Low_level.Flags.Open.(generic_read + synchronise)
        Low_level.Flags.Disposition.open_
        Low_level.Flags.Create.non_directory
    in
    (Flow.of_fd fd :> Eio.File.ro_ty Eio.Resource.t)

  let open_out t ~sw ~follow ~append ~create path =
    let disp =
      match create with
      | `Never         -> Low_level.Flags.Disposition.open_
      | `If_missing  _ -> Low_level.Flags.Disposition.open_if
      | `Or_truncate _ -> Low_level.Flags.Disposition.overwrite_if
      | `Exclusive   _ -> Low_level.Flags.Disposition.create
    in
    let flags =
      if append then Low_level.Flags.Open.(synchronise + append)
      else Low_level.Flags.Open.(generic_write + synchronise)
    in
    let fd =
      open_path t ~sw path flags disp Low_level.Flags.Create.non_directory
        ~follow:(if follow then Low_level.Follow else Nofollow)
    in
    (Flow.of_fd fd :> Eio.File.rw_ty r)

  let mkdir t ~perm path =
    with_parent_dir t path @@ fun dirfd path ->
    Err.run (Low_level.mkdir ?dirfd ~mode:perm) path

  let unlink t path =
    with_parent_dir t path @@ fun dirfd path ->
    Err.run (Low_level.unlink ?dirfd ~dir:false) path

  let rmdir t path =
    with_parent_dir t path @@ fun dirfd path ->
    Err.run (Low_level.unlink ?dirfd ~dir:true) path

  let close t =
    t.closed <- true;
    Option.iter Fd.close t.root

  let open_dir t ~sw path =
    Switch.check sw;
    let label = Nt_path.basename path in
    let root = open_dir_fd t ~sw path in
    let dir_path = Err.run Low_level.realpath_fd root in
    let d = { dir_path; sandbox = true; label; root = Some root; closed = false } in
    Switch.on_release sw (fun () -> close d);
    d

  let stat t ~follow path =
    Switch.run @@ fun sw ->
    let fd =
      open_path t ~sw path
        ~follow:(if follow then Low_level.Follow else Open_link)
        Low_level.Flags.Open.(generic_read + synchronise)
        Low_level.Flags.Disposition.open_
        Low_level.Flags.Create.empty
    in
    Flow.Impl.stat fd

  let read_dir t path =
    Switch.run @@ fun sw ->
    Err.run Low_level.readdir_fd (open_dir_fd t ~sw path) |> Array.to_list

  let with_dir_entries t path fn =
    Switch.run @@ fun sw ->
    let dir = open_dir t ~sw path in
    let entries =
      with_root dir (Err.run Low_level.readdir_fd)
      |> Array.map (fun name ->
          match stat ~follow:false dir name with
          | info -> (info.kind, name)
          | exception Eio.Exn.Io _ -> (`Unknown, name)
        )
    in
    fn (Array.to_seq entries)

  let read_link t path =
    with_parent_dir t path @@ fun dirfd path ->
    Err.run (Low_level.read_link ?dirfd) path

  let chown ~follow ?uid ?gid t path =
    with_parent_dir t path @@ fun dirfd path ->
    Err.run (fun () -> Low_level.chown ?dirfd ~follow ?uid ?gid path) ()

  let rename t old_path new_dir new_path =
    match Handler.as_posix_dir new_dir with
    | None -> invalid_arg "Target is not an eio_windows directory!"
    | Some new_dir ->
      with_parent_dir t old_path @@ fun old_dir old_path ->
      with_parent_dir new_dir new_path @@ fun new_dir new_path ->
      Err.run (Low_level.rename ?old_dir old_path ?new_dir) new_path

  (* The link's file/directory type is fixed at creation. *)
  let symlink ~link_to t path =
    let target = Err.run Low_level.symlink_target link_to in
    with_parent_dir t path @@ fun dirfd path ->
    let to_dir =
      match (
        (* Resolve from the physical parent, keeping type detection inside the sandbox. *)
        let parent =
          match dirfd with
          | Some fd -> Err.run Low_level.realpath_fd fd
          | None -> Err.run Low_level.realpath (Nt_path.dirname path)
        in
        let root =
          if t.sandbox then Some (Err.run (fun () -> with_root t Low_level.realpath_fd) ())
          else None
        in
        let target = checked t path (Nt_path.resolve_link ?root ~parent target) in
        (stat t ~follow:true target).kind
      ) with
      | `Directory -> true
      | _ | exception Eio.Io _ -> false
    in
    Err.run (Low_level.symlink_raw ~to_dir ~target dirfd) path

  let open_subtree t ~sw path = Eio.Resource.T (open_dir t ~sw path, Handler.v)

  let chmod t ~follow:_ ~perm path =
    with_parent_dir t path @@ fun dirfd path ->
    Err.run (Low_level.chmod ~mode:perm dirfd) path

  let sync_dir _t _path = ()        (* Directories do not require fsync on Windows *)

  let pp f t = Fmt.string f (String.escaped t.label)

  let native_internal t path =
    if Nt_path.is_relative path then (
      let p =
        if t.dir_path = "." then path
        else Nt_path.join t.dir_path path
      in
      if p = "" then "."
      else if p = "." then p
      else if Filename.is_implicit p then ".\\" ^ p
      else p
    ) else path

  let native t path =
    Some (native_internal t path)

  include Eio_utils.Nt_path
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

let dir ~label ~sandbox path = Eio.Resource.T (Dir.v ~label ~sandbox path, Handler.v)

let fs = Eio.Path.of_dir (dir ~label:"fs" ~sandbox:false ".")
let cwd = Eio.Path.of_dir (dir ~label:"cwd" ~sandbox:true ".")
