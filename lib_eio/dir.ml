module Unix_perm = struct
  type t = int
end

type path = string

exception Already_exists of path * exn
exception Not_found of path * exn
exception Permission_denied of path * exn

class virtual rw = object (_ : #Generic.t)
  method probe _ = None
  inherit Flow.read
  inherit Flow.write
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

let with_open_in (t:#t) path fn =
  Switch.run @@ fun sw -> fn (open_in ~sw t path)

let with_open_out ?append ~create (t:#t) path fn =
  Switch.run @@ fun sw -> fn (open_out ~sw ?append ~create t path)

let with_open_dir (t:#t) path fn =
  Switch.run @@ fun sw -> fn (open_dir ~sw t path)