(** A [Dir.t] represents access to a directory and contents, recursively.

    {!Stdenv.fs} provides access to the whole file-system.

    Example:

    {[
      Eio.Dir.load fs "/etc/passwd"
    ]}
*)

(** Tranditional Unix permissions. *)
module Unix_perm : sig
  type t = int
  (** This is the same as {!Unix.file_perm}, but avoids a dependency on [Unix]. *)
end

type path = string

exception Already_exists of path * exn
exception Not_found of path * exn
exception Permission_denied of path * exn

class virtual rw : object
  inherit Generic.t
  inherit Flow.source
  inherit Flow.sink
end

(** When to create a new file. *)
type create = [
  | `Never                            (** fail if the named file doesn't exist *)
  | `If_missing of Unix_perm.t        (** create if file doesn't already exist *)
  | `Or_truncate of Unix_perm.t       (** any existing file is truncated to zero length *)
  | `Exclusive of Unix_perm.t         (** always create; fail if the file already exists *)
]
(** If a new file is created, the given permissions are used for it. *)

class virtual t : object
  method virtual open_in : sw:Switch.t -> path -> <Flow.source; Flow.close>
  method virtual open_out :
    sw:Switch.t ->
    append:bool ->
    create:create ->
    path -> <rw; Flow.close>
  method virtual mkdir : perm:Unix_perm.t -> path -> unit
  method virtual open_dir : sw:Switch.t -> path -> t_with_close
  method virtual read_dir : path -> path list
  method virtual unlink : path -> unit
  method virtual rmdir : path -> unit
end
and virtual t_with_close : object
  inherit t
  method virtual close : unit
end

(** {1 Reading files} *)

val load : #t -> path -> string
(** [load t path] returns the contents of the given file.

    This is a convenience wrapper around {!with_open_in}. *)

val open_in : sw:Switch.t -> #t -> path -> <Flow.source; Flow.close>
(** [open_in ~sw t path] opens [t/path] for reading.

    Note: files are always opened in binary mode. *)

val with_open_in : #t -> path -> (<Flow.source; Flow.close> -> 'a) -> 'a
(** [with_open_in] is like [open_in], but calls [fn flow] with the new flow and closes
    it automatically when [fn] returns (if it hasn't already been closed by then). *)

val with_lines : #t -> path -> (string Seq.t -> 'a) -> 'a
(** [with_lines t path fn] is a convenience function for streaming the lines of the file.

    It uses {!Buf_read.lines}. *)

(** {1 Writing files} *)

val save : ?append:bool -> create:create -> #t -> path -> string -> unit
(** [save t path data ~create] writes [data] to [path].

    This is a convenience wrapper around {!with_open_out}. *)

val open_out :
  sw:Switch.t ->
  ?append:bool ->
  create:create ->
  #t -> path -> <rw; Flow.close>
(** [open_out ~sw t path] opens [t/path] for reading and writing.

    Note: files are always opened in binary mode.
    @param append Open for appending: always write at end of file.
    @param create Controls whether to create the file, and what permissions to give it if so. *)

val with_open_out :
  ?append:bool ->
  create:create ->
  #t -> path -> (<rw; Flow.close> -> 'a) -> 'a
(** [with_open_out] is like [open_out], but calls [fn flow] with the new flow and closes
    it automatically when [fn] returns (if it hasn't already been closed by then). *)

(** {1 Directories} *)

val mkdir : #t -> perm:Unix_perm.t -> path -> unit
(** [mkdir t ~perm path] creates a new directory [t/path] with permissions [perm]. *)

val open_dir : sw:Switch.t -> #t -> path -> <t; Flow.close>
(** [open_dir ~sw t path] opens [t/path].

    This can be passed to functions to grant access only to the subtree [t/path]. *)

val with_open_dir : #t -> path -> (<t; Flow.close> -> 'a) -> 'a
(** [with_open_dir] is like [open_dir], but calls [fn dir] with the new directory and closes
    it automatically when [fn] returns (if it hasn't already been closed by then). *)

val read_dir : #t -> path -> string list
(** [read_dir t path] reads directory entries for [t/path]. The entries are sorted using {! String.compare}.*)

(** {1 Other} *)

val unlink : #t -> path -> unit
(** [unlink t path] removes directory entry [t/path].

    Note: this cannot be used to unlink directories.
    Use {!rmdir} for directories. *)

val rmdir : #t -> path -> unit
(** [rmdir t path] removes directory entry [t/path].
    This only works when the entry is itself a directory.

    Note: this usually requires the directory to be empty. *)
