(** File-system access on Windows.

    {2 Paths}

    Programs normally use Win32 paths ([C:\a\b], [\\server\share\a]), which Eio
    converts to the NT paths that the kernel uses ([\??\C:\a\b]) itself, so that it
    can open files relative to a directory handle. See {!Eio_utils.Nt_path} for the
    details, including verbatim ([\\?\...]) paths and reserved names such as [NUL].

    {2 Sandboxes}

    [cwd] and directories opened with {!Eio.Path.open_dir} only give access to the
    files beneath them, as on other platforms. Windows differs from POSIX in some ways:

    - Relative component names are literal, including trailing dots and spaces.
      Names returned by [read_dir] therefore identify the same objects when used
      in subsequent operations. Unrestricted [fs] paths use Win32 normalisation.

    - [..] is applied to the path as written, before following any links, as Win32 does.
      So [link\..\x] is always [x], wherever [link] points.

    - A path may not use [..] to leave the directory, even to come back again.

    - Absolute paths are refused, but a link may have an absolute target if it is within
      the directory (junctions always do).

    - Relative link targets ending in names made only of dots and spaces (other
      than [.] and [..]) are refused: Windows interprets these differently for file
      and directory links. Such names can still be accessed directly.

    Opened subtrees retain a directory handle. Listing and symlink creation use
    handles too: an open directory can still be changed into a junction, so its
    path is not a safe substitute for its handle.
    [cwd] tracks changes to the process's current directory, but refuses access
    if acquiring that directory would follow a reparse point (including in an ancestor).

    {2 Links}

    Windows has two kinds of link, both built on {e reparse points}: data attached
    to a file or directory that tells Windows to look elsewhere.

    - {e Symbolic links} are like POSIX ones, but are either to a file or to a
      directory, fixed when the link is made. {!Eio.Path.symlink} picks whichever
      the target is at that time (or a file link if it doesn't exist yet). Making
      them needs the [SeCreateSymbolicLinkPrivilege] privilege (which administrators have)
      or Developer Mode. Their targets may be relative.

    - {e Junctions} always link to a directory, by its absolute path. Many Windows
      tools create them (e.g. [mklink /J]), as they need no privileges.

    Eio treats both as symbolic links: {!Eio.Path.stat} reports them as
    [`Symbolic_link] (without following them), {!Eio.Path.read_link} gives their
    target, and {!Eio.Path.unlink} and {!Eio.Path.rmtree} remove the link rather than
    what it points to. Other reparse points (such as OneDrive placeholders) are not
    links, and Eio treats them as ordinary files.

    {2 Other differences from Unix}

    - Names are case-insensitive.
    - Unix permissions are mostly ignored, and [chown] is not supported.
    - A link to a directory is itself a directory, so {!Eio.Path.rmdir} also removes one. *)

open Eio.Std

module rec Dir : sig
  include Eio.Fs.Pi.DIR

  val v : label:string -> sandbox:bool -> string -> t
  (** [v ~label ~sandbox dir_path] is the directory [dir_path].
      If [sandbox] is [true], it only gives access to things beneath [dir_path]. *)

  val resolve : t -> string -> string
  (** [resolve t path] returns the real path that should be used to access [path].
      For sandboxes, this is an absolute path with any links followed (and it checks that it is within the sandbox).
      For unrestricted access, this returns [path] unchanged.
      The returned name is a snapshot, not a safe way to reopen the object in a sandbox.
      @raise Eio.Fs.Permission_denied if sandboxed and [path] is outside of [dir_path]. *)

  val with_parent_dir : t -> string -> (Eio_unix.Fd.t option -> string -> 'a) -> 'a
  (** [with_parent_dir t path fn] runs [fn dir_fd rel_path],
      where [rel_path] accessed relative to [dir_fd] gives access to [path].
      For unrestricted access, this just runs [fn None path].
      For sandboxes, it opens the parent of [path] as [dir_fd] and runs [fn (Some dir_fd) (basename path)]. *)
end

and Handler : sig
  val v : (Dir.t, [`Dir | `Close]) Eio.Resource.handler

  val as_posix_dir : [> `Dir] r -> Dir.t option
  (** [as_posix_dir dir] is the {!Dir.t} behind [dir], if it is an eio_windows directory. *)
end

val dir : label:string -> sandbox:bool -> string -> [`Dir | `Close] r
(** [dir ~label ~sandbox path] is a resource for the directory [path] (see {!Dir.v}). *)

val fs : [`Dir | `Close] Eio.Path.t
(** Unrestricted access to the file system. *)

val cwd : [`Dir | `Close] Eio.Path.t
(** The current directory, as a sandbox. *)
