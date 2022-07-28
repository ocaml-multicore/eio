(** A [_ Dir.t] represents a particular directory in some filesystem.
    It is a pair of a base directory and a path relative to that location.

    {!Eio.Stdenv.cwd} provides access to the current working directory.
    For example:

    {[
      let ( / ) = Eio.Dir.( / )

      let run dir =
        Eio.Dir.save ~create:(`Exclusive 0o600)
          (dir / "output.txt") "the data"

      let () =
        Eio_main.run @@ fun env ->
        run (Eio.Stdenv.cwd env)
    ]}

    It is normally not permitted to access anything above the base directory,
    even by following a symlink.
    The exception is {!Stdenv.fs}, which provides access to the whole file-system:

    {[
      Eio.Dir.load (fs / "/etc/passwd")
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

class virtual dirfd : object
  inherit Generic.t
  method virtual open_in : sw:Switch.t -> path -> <Flow.source; Flow.close>
  method virtual open_out :
    sw:Switch.t ->
    append:bool ->
    create:create ->
    path -> <rw; Flow.close>
  method virtual mkdir : perm:Unix_perm.t -> path -> unit
  method virtual open_dir : sw:Switch.t -> path -> dirfd_with_close
  method virtual read_dir : path -> path list
  method virtual unlink : path -> unit
  method virtual rmdir : path -> unit
  method virtual rename : path -> dirfd -> path -> unit
end
and virtual dirfd_with_close : object
  inherit dirfd
  method virtual close : unit
end

type 'a t = (#dirfd as 'a) * path
(** An OS directory FD and a path relative to it, for use with e.g. [openat(2)]. *)

val ( / ) : 'a t -> string -> 'a t
(** [t / step] is [t] with [step] appended to [t]'s path,
    or replacing [t]'s path if [step] is absolute:

    - [(fd, "foo") / "bar" = (fd, "foo/bar")]
    - [(fd, "foo") / "/bar" = (fd, "/bar")] *)

val pp : _ t Fmt.t
(** [pp] formats a [_ t] as a quoted string, suitable for logging.

    Currently, it only shows the path component. *)

(** {1 Reading files} *)

val load : _ t -> string
(** [load t] returns the contents of the given file.

    This is a convenience wrapper around {!with_open_in}. *)

val open_in : sw:Switch.t -> _ t -> <Flow.source; Flow.close>
(** [open_in ~sw t] opens [t] for reading.

    Note: files are always opened in binary mode. *)

val with_open_in : _ t -> (<Flow.source; Flow.close> -> 'a) -> 'a
(** [with_open_in] is like [open_in], but calls [fn flow] with the new flow and closes
    it automatically when [fn] returns (if it hasn't already been closed by then). *)

val with_lines : _ t -> (string Seq.t -> 'a) -> 'a
(** [with_lines t fn] is a convenience function for streaming the lines of the file.

    It uses {!Buf_read.lines}. *)

(** {1 Writing files} *)

val save : ?append:bool -> create:create -> _ t -> string -> unit
(** [save t data ~create] writes [data] to [t].

    This is a convenience wrapper around {!with_open_out}. *)

val open_out :
  sw:Switch.t ->
  ?append:bool ->
  create:create ->
  _ t -> <rw; Flow.close>
(** [open_out ~sw t] opens [t] for reading and writing.

    Note: files are always opened in binary mode.
    @param append Open for appending: always write at end of file.
    @param create Controls whether to create the file, and what permissions to give it if so. *)

val with_open_out :
  ?append:bool ->
  create:create ->
  _ t -> (<rw; Flow.close> -> 'a) -> 'a
(** [with_open_out] is like [open_out], but calls [fn flow] with the new flow and closes
    it automatically when [fn] returns (if it hasn't already been closed by then). *)

(** {1 Directories} *)

val mkdir : perm:Unix_perm.t -> _ t -> unit
(** [mkdir ~perm t] creates a new directory [t] with permissions [perm]. *)

val open_dir : sw:Switch.t -> _ t -> <dirfd; Flow.close> t
(** [open_dir ~sw t] opens [t].

    This can be passed to functions to grant access only to the subtree [t]. *)

val with_open_dir : _ t -> (<dirfd; Flow.close> t -> 'a) -> 'a
(** [with_open_dir] is like [open_dir], but calls [fn dir] with the new directory and closes
    it automatically when [fn] returns (if it hasn't already been closed by then). *)

val read_dir : _ t -> string list
(** [read_dir t] reads directory entries for [t]. The entries are sorted using {! String.compare}.*)

(** {1 Other} *)

val unlink : _ t -> unit
(** [unlink t] removes directory entry [t].

    Note: this cannot be used to unlink directories.
    Use {!rmdir} for directories. *)

val rmdir : _ t -> unit
(** [rmdir t] removes directory entry [t].
    This only works when the entry is itself a directory.

    Note: this usually requires the directory to be empty. *)

val rename : _ t -> _ t -> unit
(** [rename old_t new_t] atomically unlinks [old_t] and links it as [new_t].

    If [new_t] already exists, it is atomically replaced. *)
