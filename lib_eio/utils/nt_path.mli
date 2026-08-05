(** Windows path syntax.

    - [join dir step] appends [step] to [dir] using ["\\"] as the directory separator,
      unless [dir] already ends in a separator. After a bare drive ([C:]) no
      separator is added, to keep the path drive-relative.

    - [split]: Volume prefixes ([C:], [\\server\share], [\\?\...], [\??\...]) are never split. *)

include Eio.Fs.Pi.PATH
