(** A [_ Path.t] represents a particular location in some filesystem.
    It is a pair of a base directory and a relative path from there.

    {!Eio.Stdenv.cwd} provides access to the current working directory.
    For example:

    {[
      let ( / ) = Eio.Path.( / )

      let run dir =
        Eio.Path.save ~create:(`Exclusive 0o600)
          (dir / "output.txt") "the data"

      let () =
        Eio_main.run @@ fun env ->
        run (Eio.Stdenv.cwd env)
    ]}

    It is normally not permitted to access anything above the base directory,
    even by following a symlink.
    The exception is {!Stdenv.fs}, which provides access to the whole file-system:

    {[
      Eio.Path.load (fs / "/etc/passwd")
    ]}

    In Eio, the directory separator is always "/", even on Windows.
    Use {!native} to convert to a native path.
*)

open Std
open Fs

type 'a t = 'a Fs.dir * path
(** An OS directory FD and a path relative to it, for use with e.g. [openat(2)]. *)

val ( / ) : 'a t -> string -> 'a t
(** [t / step] is [t] with [step] appended to [t]'s path,
    or replacing [t]'s path if [step] is absolute:

    - [(fd, "foo") / "bar" = (fd, "foo/bar")]
    - [(fd, "foo") / "/bar" = (fd, "/bar")] *)

val pp : _ t Fmt.t
(** [pp] formats a [_ t] as "<label:path>", suitable for logging. *)

val native : _ t -> string option
(** [native t] returns a path that can be used to refer to [t] with the host
    platform's native string-based file-system APIs, if available.
    This is intended for interoperability with non-Eio libraries.

    This does not check for confinement (the resulting path might not be accessible
    via [t] itself). Also, if a directory was opened with {!open_dir} and later
    renamed, this might use the old name.

    Using strings as paths is not secure if components in the path can be
    replaced by symlinks while the path is being used. For example, if you
    try to write to "/home/mal/output.txt" just as mal replaces "output.txt"
    with a symlink to "/etc/passwd". *)

val native_exn : _ t -> string
(** Like {!native}, but raise a suitable exception if the path is not a native path. *)

(** {1 Reading files} *)

val load : _ t -> string
(** [load t] returns the contents of the given file.

    This is a convenience wrapper around {!with_open_in}. *)

val open_in : sw:Switch.t -> _ t -> File.ro_ty r
(** [open_in ~sw t] opens [t] for reading.

    Note: files are always opened in binary mode. *)

val with_open_in : _ t -> (File.ro_ty r -> 'a) -> 'a
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
  _ t -> File.rw_ty Resource.t
(** [open_out ~sw t] opens [t] for reading and writing.

    Note: files are always opened in binary mode.
    @param append Open for appending: always write at end of file.
    @param create Controls whether to create the file, and what permissions to give it if so. *)

val with_open_out :
  ?append:bool ->
  create:create ->
  _ t -> (File.rw_ty r -> 'a) -> 'a
(** [with_open_out] is like [open_out], but calls [fn flow] with the new flow and closes
    it automatically when [fn] returns (if it hasn't already been closed by then). *)

(** {1 Directories} *)

val mkdir : perm:File.Unix_perm.t -> _ t -> unit
(** [mkdir ~perm t] creates a new directory [t] with permissions [perm]. *)

val open_dir : sw:Switch.t -> _ t -> [`Close | dir_ty] t
(** [open_dir ~sw t] opens [t].

    This can be passed to functions to grant access only to the subtree [t]. *)

val with_open_dir : _ t -> ([`Close | dir_ty] t -> 'a) -> 'a
(** [with_open_dir] is like [open_dir], but calls [fn dir] with the new directory and closes
    it automatically when [fn] returns (if it hasn't already been closed by then). *)

val read_dir : _ t -> string list
(** [read_dir t] reads directory entries for [t].

    The entries are sorted using {! String.compare}.

    Note: The special Unix entries "." and ".." are not included in the results. *)

(** {2 Metadata} *)

val exists : _ t -> bool
(** [exists t] returns [true] if [t] is a regular file or directory, and [false]
    if [t] does not exist or is some other {!type:File.kind}.
    Raises an exception if the path cannot be accessed due to permissions. *)

val is_file : _ t -> bool
(** [is_file] returns [true] if [t] is a regular file, and [false]
    if [t] does not exist or is some other {!type:File.kind}.
    Raises an exception if the path cannot be accessed due to permissions. *)

val is_directory : _ t -> bool
(** [is_directory] returns [true] if [t] is a directory, and false
    if it does not exist or is some other {!type:File.kind}.
    Raises an exception if the path cannot be accessed due to permissions. *)

val kind : follow:bool -> _ t -> File.kind
(** [kind ~follow t] returns the kind of file that [t] is.

    If [t] is a symlink, the information returned is about the target if [follow = true],
    otherwise it is about the link itself.

    Equivalent to [stat ~follow t [File.Kind] Fun.id],
    so use {!stat} to query multiple items from a file in one call. *)

val size : follow:bool -> _ t -> int64
(** [size ~follow t] returns the size of [t].

    If [t] is a symlink, the information returned is about the target if [follow = true],
    otherwise it is about the link itself.

    Equivalent to [stat ~follow t [File.Size] Fun.id],
    so use {!stat} to query multiple items from a file in one call. *)

val perm : follow:bool -> _ t -> int
(** [perm ~follow t] returns the file permissions of [t].

    If [t] is a symlink, the information returned is about the target if [follow = true],
    otherwise it is about the link itself.

    Equivalent to [stat ~follow t [File.Perm] Fun.id],
    so use {!stat} to query multiple items from a file in one call. *)

val uid : follow:bool -> _ t -> int64
(** [uid ~follow t] returns the user id associated with [t].

    If [t] is a symlink, the information returned is about the target if [follow = true],
    otherwise it is about the link itself.

    Equivalent to [stat ~follow t [File.Uid] Fun.id],
    so use {!stat} to query multiple items from a file in one call. *)

val gid : follow:bool -> _ t -> int64
(** [gid ~follow t] returns the group id associated with [t].

    If [t] is a symlink, the information returned is about the target if [follow = true],
    otherwise it is about the link itself.

    Equivalent to [stat ~follow t [File.Gid] Fun.id],
    so use {!stat} to query multiple items from a file in one call. *)

val atime : follow:bool -> _ t -> float
(** [atime ~follow t] returns the last access time of [t] as the seconds
    since the start of the epoch.

    If [t] is a symlink, the information returned is about the target if [follow = true],
    otherwise it is about the link itself.

    Equivalent to [stat ~follow t [File.Atime] Fun.id],
    so use {!stat} to query multiple items from a file in one call. *)

val mtime : follow:bool -> _ t -> float
(** [mtime ~follow t] returns the last modified time of [t] as the seconds
    since the start of the epoch.

    If [t] is a symlink, the information returned is about the target if [follow = true],
    otherwise it is about the link itself.

    Equivalent to [stat ~follow t [File.Mtime] Fun.id],
    so use {!stat} to query multiple items from a file in one call. *)

val ctime : follow:bool -> _ t -> float
(** [ctime t] returns the creation time of [t] as the seconds
    since the start of the epoch.

    If [t] is a symlink, the information returned is about the target if [follow = true],
    otherwise it is about the link itself.

    Equivalent to [stat ~follow t [File.Ctime] Fun.id],
    so use {!stat} to query multiple items from a file in one call. *)

val stat : follow:bool -> _ t -> ('a, 'b) File.stats -> 'a -> 'b
(** [stat ~follow t f k] returns metadata about the file [t], querying multiple
    fields [f] in one call and applying them to the continuation [k].

    If [t] is a symlink, the information returned is about the target if [follow = true],
    otherwise it is about the link itself.

    For example, to follow a symlink and print the kind, size and last modified
    time of its target:

    {[ stat ~follow:true t File.[ Kind; Size; Mtime ]
        (fun kind size mtime ->
          traceln "kind: %a size: %Ld mtime: %f"
            File.pp_kind size mtime)
     ]}

    If you only require access to one field, consider using simpler accessor
    functions in this module such as {!val:size} or {!val:kind}.  *)


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
