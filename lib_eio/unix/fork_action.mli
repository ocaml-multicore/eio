(** Actions to perform after forking a child process.

    To spawn a child executable on Unix, the parent forks a copy of itself,
    then has the child copy set up the environment for the new program and
    execute it.

    However, we cannot run any OCaml code in the forked child process. This is
    because `fork` only duplicates its own domain. To the child, it appears
    that all other domains have stopped responding and if it tries to e.g.
    perform a GC then the child process will hang.

    Therefore, the fork call and all child actions need to be written in C.
    This module provides some support code for doing that.
    Individual backends will wrap these actions with higher-level APIs and
    can also add their own platform-specific actions *)

type fork_fn
(** A C function, as defined in "include/fork_action.h". *)

type c_action = Obj.t
(** An action to be performed in a child process after forking.
    This must be a tuple whose first field is a [fork_fn]. *)

type t = { run : 'a. ((c_action -> 'a) -> 'a) } [@@unboxed]
(** An action that calls [run k] in the parent process to create the C action.
    [run] passes the action to [k], which forks the child and runs it. When [k]
    returns, [run] can free any resources used. *)

val with_actions : t list -> (c_action list -> 'a) -> 'a

(** {2 Actions} *)

val execve : string -> argv:string array -> env:string array -> t
(** See [execve(2)]. *)

val chdir : string -> t
(** [chdir path] changes directory to [path]. *)

val fchdir : Unix.file_descr -> t
(** [fchdir fd] changes directory to [fd]. *)
