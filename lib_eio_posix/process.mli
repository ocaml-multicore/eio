open Eio.Std

type t = {
  pid : int;
  exit_status : Unix.process_status Promise.t;  (** Resolves once the process has finished. *)
}
(** A child process. *)

(** Setup actions to perform in the child process. *)
module Fork_action : sig
  type t = Eio_unix.Private.Fork_action.t

  val execve : string -> argv:string array -> env:string array -> t
  (** See execve(2) *)

  val chdir : string -> t
  (** [chdir path] changes directory to [path]. *)

  val fchdir : Fd.t -> t
  (** [fchdir dir] changes directory to [dir]. *)
end

val spawn : sw:Switch.t -> Fork_action.t list -> t
(** [spawn ~sw actions] forks a child process, which executes [actions].
    The last action should be {!Fork_action.execve}.

    You will typically want to do [Promise.await child.exit_status] after this.

    @param sw The child will be sent {!Sys.sigkill} if [sw] finishes. *)

val signal : t -> int -> unit
(** [signal t x] sends signal [x] to [t].

    This is similar to doing [Unix.kill t.pid x],
    except that it ensures no signal is sent after [t] has been reaped. *)

(**/**)

val handle_sigchld : unit -> unit
(** For internal use only. *)
