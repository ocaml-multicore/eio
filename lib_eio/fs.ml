(** Defines types used by file-systems. *)

(** Tranditional Unix permissions. *)
module Unix_perm = struct
  type t = int
  (** This is the same as {!Unix.file_perm}, but avoids a dependency on [Unix]. *)
end

type path = string

exception Already_exists of path * exn
exception Not_found of path * exn
exception Permission_denied of path * exn

class virtual rw = object (_ : <Generic.t; Flow.source; Flow.sink; ..>)
  method probe _ = None
  method read_methods = []
end

(** When to create a new file. *)
type create = [
  | `Never                            (** fail if the named file doesn't exist *)
  | `If_missing of Unix_perm.t        (** create if file doesn't already exist *)
  | `Or_truncate of Unix_perm.t       (** any existing file is truncated to zero length *)
  | `Exclusive of Unix_perm.t         (** always create; fail if the file already exists *)
]
(** If a new file is created, the given permissions are used for it. *)

(** Note: use the functions in {!Path} to access directories. *)
class virtual dir = object (_ : #Generic.t)
  method probe _ = None
  method virtual open_in : sw:Switch.t -> path -> <Flow.source; Flow.close>
  method virtual open_out :
    sw:Switch.t ->
    append:bool ->
    create:create ->
    path -> <rw; Flow.close>
  method virtual mkdir : perm:Unix_perm.t -> path -> unit
  method virtual open_dir : sw:Switch.t -> path -> dir_with_close
  method virtual read_dir : path -> string list
  method virtual unlink : path -> unit
  method virtual rmdir : path -> unit
  method virtual rename : path -> dir -> path -> unit
  method virtual pp : Format.formatter -> unit
end
and virtual dir_with_close = object
  (* This dummy class avoids an "Error: The type < .. > is not an object type" error from the compiler. *)
  inherit dir
  method virtual close : unit
end
