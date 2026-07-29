open Eio.Std

type ty = [`Dir | `Close | `Mock]
type t = ty r

type state = {
  label : string;
  on_open_in : Eio.File.ro_ty r Handler.t;
  on_open_out : Eio.File.rw_ty r Handler.t;
  on_open_subtree : [`Close | Eio.Fs.dir_ty] r Handler.t;
  on_read_dir : string list Handler.t;
  on_stat : Eio.File.Stat.t Handler.t;
  on_read_link : string Handler.t;
}

let make_state label = {
  label;
  on_open_in = Handler.make (`Raise (Failure "Mock open_in handler not configured"));
  on_open_out = Handler.make (`Raise (Failure "Mock open_out handler not configured"));
  on_open_subtree = Handler.make (`Raise (Failure "Mock open_subtree handler not configured"));
  on_read_dir = Handler.make (`Raise (Failure "Mock read_dir handler not configured"));
  on_stat = Handler.make (`Raise (Failure "Mock stat handler not configured"));
  on_read_link = Handler.make (`Raise (Failure "Mock read_link handler not configured"));
}

let pp_create f = function
  | `Never -> Fmt.string f "`Never"
  | `If_missing perm -> Fmt.pf f "`If_missing 0o%o" perm
  | `Or_truncate perm -> Fmt.pf f "`Or_truncate 0o%o" perm
  | `Exclusive perm -> Fmt.pf f "`Exclusive 0o%o" perm

let pp_id = Fmt.option ~none:(Fmt.any "None") (fun f x -> Fmt.pf f "%Ld" x)

let close t = traceln "%s: closed" t.label

module Impl (Path_syntax : Eio.Fs.Pi.PATH) = struct
  type t = state

  let open_in t ~sw path =
    traceln "%s: open_in %S" t.label path;
    let f = Handler.run t.on_open_in in
    Switch.on_release sw (fun () -> Eio.Resource.close f);
    f

  let open_out t ~sw ~append ~create path =
    traceln "%s: open_out ~append:%b ~create:%a %S" t.label append pp_create create path;
    let f = Handler.run t.on_open_out in
    Switch.on_release sw (fun () -> Eio.Resource.close f);
    f

  let open_subtree t ~sw path =
    traceln "%s: open_subtree %S" t.label path;
    let d = Handler.run t.on_open_subtree in
    Switch.on_release sw (fun () -> Eio.Resource.close d);
    d

  let mkdir t ~perm path =
    traceln "%s: mkdir ~perm:0o%o %S" t.label perm path

  let read_dir t path =
    traceln "%s: read_dir %S" t.label path;
    Handler.run t.on_read_dir

  let with_dir_entries t path fn =
    traceln "%s: with_dir_entries %S" t.label path;
    Handler.run t.on_read_dir
    |> List.to_seq
    |> Seq.map (fun name -> (`Unknown, name))
    |> fn

  let stat t ~follow path =
    traceln "%s: stat ~follow:%b %S" t.label follow path;
    Handler.run t.on_stat

  let unlink t path =
    traceln "%s: unlink %S" t.label path

  let rmdir t path =
    traceln "%s: rmdir %S" t.label path

  let rename t old_path new_dir new_path =
    traceln "%s: rename %S to %a" t.label old_path Eio.Path.pp (new_dir, new_path)

  let read_link t path =
    traceln "%s: read_link %S" t.label path;
    Handler.run t.on_read_link

  let symlink ~link_to t path =
    traceln "%s: symlink ~link_to:%S %S" t.label link_to path

  let chmod t ~follow ~perm path =
    traceln "%s: chmod ~follow:%b ~perm:0o%o %S" t.label follow perm path

  let chown ~follow ?uid ?gid t path =
    traceln "%s: chown ~follow:%b ~uid:%a ~gid:%a %S" t.label follow pp_id uid pp_id gid path

  let pp f t = Fmt.string f (String.escaped t.label)

  let native _ _ = None

  include Path_syntax
end

module Posix_impl = Impl (Eio_utils.Posix_path)
module Nt_impl = Impl (Eio_utils.Nt_path)

type (_, _, _) Eio.Resource.pi += Raw : ('t, 't -> state, ty) Eio.Resource.pi
let raw (Eio.Resource.T (t, ops)) = Eio.Resource.get ops Raw t

let make_handler dir : (state, ty) Eio.Resource.handler =
  Eio.Resource.handler [
    H (Eio.Fs.Pi.Dir, dir);
    H (Eio.Resource.Close, close);
    H (Raw, Fun.id);
  ]

let posix_handler = make_handler (module Posix_impl : Eio.Fs.Pi.DIR with type t = state)
let nt_handler = make_handler (module Nt_impl : Eio.Fs.Pi.DIR with type t = state)

let make ?(syntax = `Posix) label : t =
  let handler =
    match syntax with
    | `Posix -> posix_handler
    | `Windows -> nt_handler
  in
  Eio.Resource.T (make_state label, handler)

let on_open_in (t:t) actions =
  let t = raw t in
  let as_file x = (x :> Eio.File.ro_ty r) in
  Handler.seq t.on_open_in (List.map (Action.map as_file) actions)

let on_open_out (t:t) actions =
  let t = raw t in
  let as_file x = (x :> Eio.File.rw_ty r) in
  Handler.seq t.on_open_out (List.map (Action.map as_file) actions)

let on_open_subtree (t:t) actions =
  let t = raw t in
  let as_dir x = (x :> [`Close | Eio.Fs.dir_ty] r) in
  Handler.seq t.on_open_subtree (List.map (Action.map as_dir) actions)

let on_read_dir (t:t) actions = Handler.seq (raw t).on_read_dir actions
let on_stat (t:t) actions = Handler.seq (raw t).on_stat actions
let on_read_link (t:t) actions = Handler.seq (raw t).on_read_link actions
