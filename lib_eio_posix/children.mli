(** Keep track of child processes and response to SIGCHLD. *)

val with_lock : (unit -> 'a) -> 'a
(** This must be held during the fork, register sequence
    (so that we don't try to reap the process before it's registered),
    and also when signalling a child process
    (to ensure it isn't reaped at the same time). *)

val register : int -> Unix.process_status Eio.Promise.t
(** [register pid] adds [pid] to the list of children and returns a promise for its exit status.
    You must hold the lock while forking and then calling this. *)

val handle_sigchld : unit -> unit
(** Call this on [SIGCHLD]. *)
