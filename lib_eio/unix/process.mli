(** This extends the {!Eio.Process} API with more control over file-descriptors. *)

open Eio.Std

(** {2 Types}

    These extend the types in {!Eio.Process} with support for file descriptors. *)

type ty = [ `Generic | `Unix ] Eio.Process.ty
type 'a t = ([> ty] as 'a) r

type mgr_ty = [`Generic | `Unix] Eio.Process.mgr_ty
type 'a mgr = ([> mgr_ty] as 'a) r

module Pi : sig
  module type MGR = sig
    include Eio.Process.Pi.MGR

    val spawn_unix :
      t ->
      sw:Switch.t ->
      ?cwd:Eio.Fs.dir_ty Eio.Path.t ->
      env:string array ->
      fds:(int * Fd.t * Fork_action.blocking) list ->
      executable:string ->
      string list ->
      ty r
  end

  type (_, _, _) Eio.Resource.pi +=
    | Mgr_unix : ('t, (module MGR with type t = 't), [> mgr_ty]) Eio.Resource.pi

  val mgr_unix :
    (module MGR with type t = 't and type tag = 'tag) ->
    ('t, 'tag Eio.Process.mgr_ty) Eio.Resource.handler
end

module Make_mgr (X : sig
  type t

  val spawn_unix :
    t ->
    sw:Switch.t ->
    ?cwd:Eio.Fs.dir_ty Eio.Path.t ->
    env:string array ->
    fds:(int * Fd.t * Fork_action.blocking) list ->
    executable:string ->
    string list ->
    ty r
end) : Pi.MGR with type t = X.t and type tag = [`Generic | `Unix]

val spawn_unix :
    sw:Switch.t ->
    _ mgr ->
    ?cwd:Eio.Fs.dir_ty Eio.Path.t ->
    fds:(int * Fd.t * Fork_action.blocking) list ->
    ?env:string array ->
    ?executable:string ->
    string list ->
    ty r
(** [spawn_unix ~sw mgr ~fds args] spawns a child process running the command [args].

    The arguments are as for {!Eio.Process.spawn},
    except that it takes a list of FD mappings for {!Private.Fork_action.inherit_fds}
    directly, rather than just flows for the standard streams. *)

val sigchld : Eio.Condition.t
(** {b If} an Eio backend installs a SIGCHLD handler, the handler will broadcast on this condition.

    This allows non-Eio libraries (such as Lwt) to share its signal handler.

    Note: Not all backends install a handler (e.g. eio_linux uses process descriptors instead),
    so be sure to call {!install_sigchld_handler} if you need to use this. *)

val install_sigchld_handler : unit -> unit
(** [install_sigchld_handler ()] sets the signal handler for SIGCHLD to broadcast {!sigchld}. *)
