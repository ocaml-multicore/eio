val enter : unit -> (unit, [`Not_supported]) result
(** Call {{:https://man.freebsd.org/cgi/man.cgi?query=cap_enter}cap_enter}.

    Once in capability mode, access to global name spaces, such as file system
    or IPC name spaces, is prevented by the operating system. A program can call
    this after opening any directories, files or network sockets that it will need,
    to prevent accidental access to other resources.

    The standard environment directories {!Eio.Stdenv.fs} and {!Eio.Stdenv.cwd} cannot
    be used after calling this, but directories opened from them via {!Eio.Path.with_open_dir}
    will continue to work. *)
