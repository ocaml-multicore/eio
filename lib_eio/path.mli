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

val split : 'a t -> ('a t * string) option
(** [split t] returns [Some (dir, basename)], where [basename] is the last path component in [t]
    and [dir] is [t] without [basename].

    [dir / basename] refers to the same path as [t].

    [split t = None] if there is nothing to split.

    For example:

    - [split (root, "foo/bar") = Some ((root, "foo"), "bar")]
    - [split (root, "/foo/bar") = Some ((root, "/foo"), "bar")]
    - [split (root, "/foo/bar/baz") = Some ((root, "/foo/bar"), "baz")]
    - [split (root, "/foo/bar//baz/") = Some ((root, "/foo/bar"), "baz")]
    - [split (root, "bar") = Some ((root, ""), "bar")]
    - [split (root, ".") = Some ((root, ""), ".")]
    - [split (root, "") = None]
    - [split (root, "/") = None]
*)

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

val mkdirs : ?exists_ok:bool -> perm:File.Unix_perm.t -> _ t -> unit
(** [mkdirs ~perm t] creates directory [t] along with any missing ancestor directories, recursively.

    All created directories get permissions [perm], but existing directories do not have their permissions changed.

    @param exist_ok If [false] (the default) then we raise {! Fs.Already_exists} if [t] is already a directory. *)

val open_dir : sw:Switch.t -> _ t -> [< `Close | dir_ty] t
(** [open_dir ~sw t] opens [t].

    This can be passed to functions to grant access only to the subtree [t]. *)

val with_open_dir : _ t -> ([< `Close | dir_ty] t -> 'a) -> 'a
(** [with_open_dir] is like [open_dir], but calls [fn dir] with the new directory and closes
    it automatically when [fn] returns (if it hasn't already been closed by then). *)

val read_dir : _ t -> string list
(** [read_dir t] reads directory entries for [t].

    The entries are sorted using {! String.compare}.

    Note: The special Unix entries "." and ".." are not included in the results. *)

(** {1 Metadata} *)

val stat : follow:bool -> _ t -> File.Stat.t
(** [stat ~follow t] returns metadata about the file [t].

    If [t] is a symlink, the information returned is about the target if [follow = true],
    otherwise it is about the link itself. *)

val kind : follow:bool -> _ t -> [ File.Stat.kind | `Not_found ]
(** [kind ~follow t] is the type of [t], or [`Not_found] if it doesn't exist.

    @param follow If [true] and [t] is a symlink, return the type of the target rather than [`Symbolic_link]. *)

val is_file : _ t -> bool
(** [is_file t] is [true] if [t] is a regular file, and [false] if it doesn't exist or has a different type.

    [is_file t] is [kind ~follow:true t = `Regular_file]. *)

val is_directory : _ t -> bool
(** [is_directory t] is [true] if [t] is a directory, and [false] if it doesn't exist or has a different type.

    [is_directory t] is [kind ~follow:true t = `Directory]. *)

val read_link : _ t -> string
(** [read_link t] is the target of symlink [t]. *)

(** {1 Other} *)

val unlink : _ t -> unit
(** [unlink t] removes directory entry [t].

    Note: this cannot be used to unlink directories.
    Use {!rmdir} for directories. *)

val rmdir : _ t -> unit
(** [rmdir t] removes directory entry [t].
    This only works when the entry is itself a directory.

    Note: this usually requires the directory to be empty. *)

val rmtree : ?missing_ok:bool -> _ t -> unit
(** [rmtree t] removes [t] (and its contents, recursively, if it's a directory).

    @param missing_ok If [false] (the default), raise an {!Fs.Not_found} IO error if [t] doesn't exist.
                      If [true], ignore missing items.
                      This applies recursively, allowing two processes
                      to attempt to remove a tree at the same time. *)

val rename : _ t -> _ t -> unit
(** [rename old_t new_t] atomically unlinks [old_t] and links it as [new_t].

    If [new_t] already exists, it is atomically replaced. *)

val symlink : ?exists_ok:bool -> target:_ t -> string -> unit
(** [symlink ~target name] creates a link to [target] called [name]. 
    
    The following program creates a new file called [./hello.txt] and then creates
    a symbolic link from [./world.txt] to [./hello.txt].
    {[
        Eio.Path.(save ~create:(`If_missing 0o644) (env#cwd / "hello.txt")) "Hello World!";
        Eio.Path.(symlink ~target:(env#cwd / "hello.txt") "world.txt")
    ]}

    @param exist_ok If [false] (the default) then we raise {! Fs.Already_exists} if the symlink already exists with that target.
*)
