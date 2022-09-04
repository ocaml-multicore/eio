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

class virtual ro = object (_ : <Generic.t; Flow.source; ..>)
  method virtual pread : file_offset:Optint.Int63.t -> Cstruct.t list -> int
end

class virtual rw = object (_ : <Generic.t; Flow.source; Flow.sink; ..>)
  inherit ro
  method virtual pwrite : file_offset:Optint.Int63.t -> Cstruct.t list -> int
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
  method virtual open_in : sw:Switch.t -> path -> <ro; Flow.close>
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

let pread (t : #ro) ~file_offset bufs =
  let got = t#pread ~file_offset bufs in
  assert (got > 0 && got <= Cstruct.lenv bufs);
  got

let rec pread_exact (t : #ro) ~file_offset bufs =
  if Cstruct.lenv bufs > 0 then (
    let got = t#pread ~file_offset bufs in
    let file_offset = Optint.Int63.add file_offset (Optint.Int63.of_int got) in
    pread_exact t ~file_offset (Cstruct.shiftv bufs got)
  )

let pwrite (t : #rw) ~file_offset bufs =
  let got = t#pwrite ~file_offset bufs in
  assert (got > 0 && got <= Cstruct.lenv bufs);
  got

let rec pwrite_exact (t : #rw) ~file_offset bufs =
  if Cstruct.lenv bufs > 0 then (
    let got = t#pwrite ~file_offset bufs in
    let file_offset = Optint.Int63.add file_offset (Optint.Int63.of_int got) in
    pwrite_exact t ~file_offset (Cstruct.shiftv bufs got)
  )
