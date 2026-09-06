(** Windows path syntax.

    - [join dir step] appends [step] to [dir] using ["\\"] as the directory separator,
      unless [dir] already ends in a separator. After a bare drive ([C:]) no
      separator is added, to keep the path drive-relative.

    - [split]: Volume prefixes ([C:], [\\server\share], [\\?\...], [\??\...]) are never split. *)

include Eio.Fs.Pi.PATH

val to_nt : cwd:string -> string -> string
(** [to_nt ~cwd path] is the NT object-manager form of the Win32 path [path].

    A relative [path] is resolved against [cwd] and, as in Win32, ["/"] is a
    separator and ["."] and [".."] components are removed. Verbatim ([\\?\])
    and NT ([\??\]) paths are passed through unchanged. *)
