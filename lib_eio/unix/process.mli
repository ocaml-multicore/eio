(** This extends the {!Eio.Process} API with more control over file-descriptors. *)

open Eio.Std

class virtual mgr : object
  inherit Eio.Process.mgr

  method pipe :
    sw:Switch.t ->
    <Eio.Flow.source; Eio.Flow.close> * <Eio.Flow.sink; Eio.Flow.close>

  method virtual spawn_unix :
    sw:Switch.t ->
    ?cwd:Eio.Fs.dir Eio.Path.t ->
    env:string array ->
    fds:(int * Fd.t * Fork_action.blocking) list ->
    executable:string ->
    string list ->
    Eio.Process.t

  method spawn :
    sw:Switch.t ->
    ?cwd:Eio.Fs.dir Eio.Path.t ->
    ?stdin:Eio.Flow.source ->
    ?stdout:Eio.Flow.sink ->
    ?stderr:Eio.Flow.sink ->
    ?env:string array ->
    ?executable:string ->
    string list ->
    Eio.Process.t
    (** The default implementation uses {!spawn_unix}. *)
end

val spawn_unix :
    sw:Switch.t ->
    #mgr ->
    ?cwd:Eio.Fs.dir Eio.Path.t ->
    fds:(int * Fd.t * Fork_action.blocking) list ->
    ?env:string array ->
    ?executable:string ->
    string list ->
    Eio.Process.t
(** [spawn_unix ~sw mgr ~fds args] spawns a child process running the command [args].

    The arguments are as for {!Eio.Process.spawn},
    except that it takes a list of FD mappings for {!Fork_action.inherit_fds}
    directly, rather than just flows for the standard streams. *)

val sigchld : Eio.Condition.t
(** {b If} an Eio backend installs a SIGCHLD handler, the handler will broadcast on this condition.

    This allows non-Eio libraries (such as Lwt) to share its signal handler.

    Note: Not all backends install a handler (e.g. eio_linux uses process descriptors instead),
    so be sure to call {!install_sigchld_handler} if you need to use this. *)

val install_sigchld_handler : unit -> unit
(** [install_sigchld_handler ()] sets the signal handler for SIGCHLD to broadcast {!sigchld}. *)
