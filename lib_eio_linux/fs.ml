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
module Fd = Eio_unix.Fd

(* When renaming, we get a plain [Eio.Fs.dir]. We need extra access to check
   that the new location is within its sandbox. *)
type ('t, _, _) Eio.Resource.pi += Dir_fd : ('t, 't -> Low_level.dir_fd, [> `Dir_fd]) Eio.Resource.pi

let get_dir_fd_opt (Eio.Resource.T (t, ops)) =
  match Eio.Resource.get_opt ops Dir_fd with
  | Some f -> Some (f t)
  | None -> None

(* fchdir wants just a directory FD, not an FD and a path like the *at functions. *)
let with_dir dir_fd path fn =
  Switch.run ~name:"with_dir" @@ fun sw ->
  Low_level.openat ~sw
    ~seekable:false
    ~access:`R
    ~perm:0
    ~flags:Uring.Open_flags.(cloexec + path + directory)
    dir_fd (if path = "" then "." else path)
  |> fn

module rec Dir : sig
  include Eio.Fs.Pi.DIR

  val v : label:string -> path:string -> Low_level.dir_fd -> t

  val close : t -> unit

  val fd : t -> Low_level.dir_fd
end = struct
  type t = {
    fd : Low_level.dir_fd;
    label : string;
    path : string;
  }

  let v ~label ~path fd = { fd; label; path }

  let open_in t ~sw path =
    let fd = Low_level.openat ~sw t.fd path
        ~access:`R
        ~flags:Uring.Open_flags.cloexec
        ~perm:0
    in
    (Flow.of_fd fd :> Eio.File.ro_ty r)

  let open_out t ~sw ~append ~create path =
    let perm, flags =
      match create with
      | `Never            -> 0,    Uring.Open_flags.empty
      | `If_missing  perm -> perm, Uring.Open_flags.creat
      | `Or_truncate perm -> perm, Uring.Open_flags.(creat + trunc)
      | `Exclusive   perm -> perm, Uring.Open_flags.(creat + excl)
    in
    let flags = if append then Uring.Open_flags.(flags + append) else flags in
    let fd = Low_level.openat ~sw t.fd path
        ~access:`RW
        ~flags:Uring.Open_flags.(cloexec + flags)
        ~perm
    in
    (Flow.of_fd fd :> Eio.File.rw_ty r)

  let native_internal t path =
    if Filename.is_relative path then (
      let p = Filename.concat t.path path in
      if p = "" then "."
      else if p = "." then p
      else if Filename.is_implicit p then "./" ^ p
      else p
    ) else path

  let open_subtree t ~sw path =
    let fd = Low_level.openat ~sw ~seekable:false t.fd (if path = "" then "." else path)
        ~access:`R
        ~flags:Uring.Open_flags.(cloexec + path + directory)
        ~perm:0
    in
    let label = Filename.basename path in
    let d = v ~label ~path:(native_internal t path) (Low_level.FD fd) in
    Eio.Resource.T (d, Dir_handler.v)

  let mkdir t ~perm path = Low_level.mkdir ~perm t.fd path

  let read_dir t path =
    Switch.run ~name:"read_dir" @@ fun sw ->
    let path = if path = "" then "." else path in
    let fd =
      Low_level.openat ~sw t.fd path
        ~seekable:false
        ~access:`R
        ~flags:Uring.Open_flags.(cloexec + directory)
        ~perm:0
    in
    Low_level.read_dir fd

  let with_dir_entries t path fn =
    Switch.run ~name:"with_dir_entries" @@ fun sw ->
    let path = if path = "" then "." else path in
    let fd =
      Low_level.openat ~sw t.fd path
        ~seekable:false
        ~access:`R
        ~flags:Uring.Open_flags.(cloexec + directory)
        ~perm:0
    in
    let rec read_entries fd : (Eio.File.Stat.kind * string) Seq.t =
      let entries = Low_level.read_some_dir fd in
      match entries with
      | [] -> fun () -> Seq.Nil
      | es ->
          let rec loop = function
            | [] -> read_entries fd
            | e :: es -> fun () -> Seq.Cons (e, loop es)
          in
          loop es
    in
    fn (read_entries fd)

  let read_link t path = Low_level.read_link t.fd path

  let chown ~follow ?uid ?gid t path = 
    Low_level.chown ~follow ?uid ?gid t.fd path

  let close t =
    match t.fd with
    | FD x -> Fd.close x
    | Cwd | Fs -> failwith "Can't close non-FD directory!"

  let unlink t path = Low_level.unlink ~rmdir:false t.fd path
  let rmdir t path = Low_level.unlink ~rmdir:true t.fd path

  let stat t ~follow path =
    let module X = Uring.Statx in
    let x = X.create () in
    Low_level.statx ~follow ~mask:X.Mask.basic_stats t.fd path x;
    Low_level.eio_of_statx x

  let rename t old_path t2 new_path =
    match get_dir_fd_opt t2 with
    | Some fd2 -> Low_level.rename t.fd old_path fd2 new_path
    | None -> raise (Unix.Unix_error (Unix.EXDEV, "rename-dst", new_path))

  let symlink ~link_to t path =
    Low_level.symlink ~link_to t.fd path

  let chmod t ~follow ~perm path =
    Low_level.chmod t.fd ~follow ~mode:perm path

  let pp f t = Fmt.string f (String.escaped t.label)

  let fd t = t.fd

  let native t path =
    Some (native_internal t path)
end
and Dir_handler : sig
  val v : (Dir.t, [`Dir | `Close]) Eio.Resource.handler
end = struct
  let v = Eio.Resource.handler [
      H (Eio.Fs.Pi.Dir, (module Dir));
      H (Eio.Resource.Close, Dir.close);
      H (Dir_fd, Dir.fd);
    ]
end

let dir ~label ~path fd = Eio.Resource.T (Dir.v ~label ~path fd, Dir_handler.v)
