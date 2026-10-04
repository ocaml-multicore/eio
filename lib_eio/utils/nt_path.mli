(** Windows path syntax.

    In Windows, programs usually use Win32 paths, which the userspace maps to
    NT object-manager paths for kernel (e.g. [C:\a] becomes [\??\C:\a]).
    For sandboxing purposes, Eio opens files using [NtCreateFile] and handles the mapping
    itself, following {{:https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file}
    Microsoft docs on Naming Files, Paths, and Namespaces} and
    {{:https://learn.microsoft.com/en-us/dotnet/standard/io/file-path-formats}
    Microsoft file path formats on Windows systems}, except where noted:

    - Both backslash and slash are separators, except in verbatim paths ([\\?\...] or
      [\??\...]).

    - [.] and [..] are resolved, and trailing dots and spaces trimmed, as in Win32.
      So, as in Win32, a name ending in a dot or space (such as [...]) can only be
      reached with a verbatim path. Unlike Win32, a trailing separator is dropped.

    - A path such as [C:a] is relative to drive [C]'s current directory.
      Only the current drive's is known here, so for other drives we use the root.

    - Reserved names such as [NUL] and [COM1] name devices in any Win32 directory.
      {!to_win32} keeps paths using them verbatim.

    - Names are case-insensitive, but {!beneath} only ignores ASCII case and
      doesn't understand short (8.3) names yet. Unknown ones are rejected though,
      so this should be safe for sandboxing purposes.

    - Paths may be longer than [MAX_PATH]. For Win32 paths we assume the application
      {{:https://learn.microsoft.com/en-us/windows/win32/fileio/maximum-file-path-limitation?tabs=registry#enable-long-paths-in-windows-10-version-1607-and-later}
      enables long paths}.

    - Reserved characters (such as [*], [?] and control characters) are left for
      the file system to reject. A [:] may also name an alternate data stream.

    For {!Eio.Path}:

    - [join dir step] appends [step] to [dir] using ["\\"] as the directory separator,
      unless [dir] already ends in a separator. After a bare drive ([C:]) no
      separator is added, to keep the path drive-relative.

    - [split]: Volume prefixes ([C:], [\\server\share], [\\?\...], [\??\...]) are never split. *)

include Eio.Fs.Pi.PATH

(** The type of a path, identified from its prefix as described in
    {{:https://learn.microsoft.com/en-us/dotnet/standard/io/file-path-formats#identify-the-path}
    Identify the path}. *)
type kind = [
  | `Relative        (** [a\b], relative to the current directory. *)
  | `Rooted          (** [\a], relative to the root of the current drive. *)
  | `Drive_relative  (** [C:a], relative to drive [C]'s current directory. *)
  | `Absolute        (** [C:\a], a fully qualified DOS path. *)
  | `Unc             (** [\\server\share\a]. *)
  | `Device          (** [\\.\pipe\a], a device path, which is normalised. *)
  | `Verbatim        (** [\\?\C:\a] or [\??\C:\a], which is not normalised.
                         Only these exact prefixes are verbatim: [//?/C:/a] is a device path. *)
]

val pp_kind : kind Fmt.t

val classify : string -> kind
(** [classify p] is the type of path [p]. *)

val is_relative : string -> bool
(** [is_relative p] is [classify p = `Relative]. *)

val dirname : string -> string
(** [dirname p] is the directory part of [p]. It is ["."] when [p] names
    something in the current directory, and also when [p] has no parent to
    name (e.g. an empty path, a bare volume ([C:]) and a root ([\\server\share])). *)

val basename : string -> string
(** [basename p] is the final component of [p]. It is [p] itself when [p] has
    no directory part, and ["."] when [p] is empty. *)

val to_nt : cwd:string -> string -> string
(** [to_nt ~cwd path] is the NT object-manager form of the Win32 path [path],
    resolved against [cwd] and normalised as described above.
    Verbatim paths are passed through unchanged. *)

val to_win32 : string -> string
(** [to_win32 p] is the verbatim path [p] without its prefix. *)

val beneath : root:string -> string -> string list option
(** [beneath ~root p] is the components of [p] below the absolute directory
    [root], or [None] if [p] is not within [root].

    A relative [p] is normalised via {!to_nt}, except that [".."] may not
    leave [root], even temporarily. Otherwise, [p] and [root] are compared in
    NT form. *)
