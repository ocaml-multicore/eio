module Unix_perm = struct
  type t = int
end

type path = string

exception Already_exists of path * exn
exception Not_found of path * exn
exception Permission_denied of path * exn

class virtual rw = object (_ : <Generic.t; Flow.source; Flow.sink; ..>)
  method probe _ = None
  method read_methods = []
end

type create = [`Never | `If_missing of Unix_perm.t | `Or_truncate of Unix_perm.t | `Exclusive of Unix_perm.t]

class virtual dirfd = object (_ : #Generic.t)
  method probe _ = None
  method virtual open_in : sw:Switch.t -> path -> <Flow.source; Flow.close>
  method virtual open_out :
    sw:Switch.t ->
    append:bool ->
    create:create ->
    path -> <rw; Flow.close>
  method virtual mkdir : perm:Unix_perm.t -> path -> unit
  method virtual open_dir : sw:Switch.t -> path -> dirfd_with_close
  method virtual read_dir : path -> string list
  method virtual unlink : path -> unit
  method virtual rmdir : path -> unit
  method virtual rename : path -> dirfd -> path -> unit
  method virtual pp : Format.formatter -> unit
end
and virtual dirfd_with_close = object
  (* This dummy class avoids an "Error: The type < .. > is not an object type" error from the compiler. *)
  inherit dirfd
  method virtual close : unit
end

type 'a t = (#dirfd as 'a) * path

let ( / ) (dir, p1) p2 =
  match p1, p2 with
  | p1, "" -> (dir, Filename.concat p1 p2)
  | _, p2 when not (Filename.is_relative p2) -> (dir, p2)
  | ".", p2 -> (dir, p2)
  | p1, p2 -> (dir, Filename.concat p1 p2)

let pp f ((t:#dirfd), p) =
  Fmt.pf f "<%t:%s>" t#pp (String.escaped p)

let open_in ~sw ((t:#dirfd), path) = t#open_in ~sw path
let open_out ~sw ?(append=false) ~create ((t:#dirfd), path) = t#open_out ~sw ~append ~create path
let open_dir ~sw ((t:#dirfd), path) = (t#open_dir ~sw path, "")
let mkdir ~perm ((t:#dirfd), path) = t#mkdir ~perm path
let read_dir ((t:#dirfd), path) = List.sort String.compare (t#read_dir path)

let with_open_in path fn =
  Switch.run @@ fun sw -> fn (open_in ~sw path)

let with_open_out ?append ~create path fn =
  Switch.run @@ fun sw -> fn (open_out ~sw ?append ~create path)

let with_open_dir path fn =
  Switch.run @@ fun sw -> fn (open_dir ~sw path)

let with_lines path fn =
  with_open_in path @@ fun flow ->
  let buf = Buf_read.of_flow flow ~max_size:max_int in
  fn (Buf_read.lines buf)

let load path =
  with_open_in path @@ fun flow ->
  let buf = Buf_read.of_flow flow ~max_size:max_int in
  Buf_read.take_all buf

let save ?append ~create path data =
  with_open_out ?append ~create path @@ fun flow ->
  Flow.copy_string data flow

let unlink ((t:#dirfd), path) = t#unlink path
let rmdir ((t:#dirfd), path) = t#rmdir path
let rename ((t1:#dirfd), old_path) (t2, new_path) = t1#rename old_path (t2 :> dirfd) new_path
