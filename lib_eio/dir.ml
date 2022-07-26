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

class virtual t = object
  method virtual open_in : sw:Switch.t -> path -> <Flow.source; Flow.close>
  method virtual open_out :
    sw:Switch.t ->
    append:bool ->
    create:create ->
    path -> <rw; Flow.close>
  method virtual mkdir : perm:Unix_perm.t -> path -> unit
  method virtual open_dir : sw:Switch.t -> path -> t_with_close
  method virtual read_dir : path -> string list
  method virtual unlink : path -> unit
  method virtual rmdir : path -> unit
end
and virtual t_with_close = object
  (* This dummy class avoids an "Error: The type < .. > is not an object type" error from the compiler. *)
  inherit t
  method virtual close : unit
end

let open_in ~sw (t:#t) = t#open_in ~sw
let open_out ~sw ?(append=false) ~create (t:#t) path = t#open_out ~sw ~append ~create path
let open_dir ~sw (t:#t) = t#open_dir ~sw
let mkdir (t:#t) = t#mkdir
let read_dir (t:#t) path = List.sort String.compare (t#read_dir path)

let with_open_in (t:#t) path fn =
  Switch.run @@ fun sw -> fn (open_in ~sw t path)

let with_open_out ?append ~create (t:#t) path fn =
  Switch.run @@ fun sw -> fn (open_out ~sw ?append ~create t path)

let with_open_dir (t:#t) path fn =
  Switch.run @@ fun sw -> fn (open_dir ~sw t path)

let with_lines (t:#t) path fn =
  with_open_in t path @@ fun flow ->
  let buf = Buf_read.of_flow flow ~max_size:max_int in
  fn (Buf_read.lines buf)

let load (t:#t) path =
  with_open_in t path @@ fun flow ->
  let buf = Buf_read.of_flow flow ~max_size:max_int in
  Buf_read.take_all buf

let save ?append ~create (t:#t) path data =
  with_open_out ?append ~create t path @@ fun flow ->
  Flow.copy_string data flow

let unlink (t:#t) path = t#unlink path
let rmdir (t:#t) path = t#rmdir path
