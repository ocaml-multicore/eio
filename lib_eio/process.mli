(** Example:
    {[
      # Eio_main.run @@ fun env ->
        let proc_mgr = Eio.Stdenv.process_mgr env in
        Eio.Process.parse_out proc_mgr Eio.Buf_read.line ["echo"; "hello"]
    ]}
 *)

open Std

(** {2 Status and error types} *)

type exit_status = [
  | `Exited of int      (** Process exited with the given return code. *)
  | `Signaled of int    (** Process was killed by the given signal. *)
]

type status = [
  | exit_status
  | `Stopped of int     (** Process was stopped (paused) by the given signal. *)
]

val pp_status : [< status] Fmt.t

type error =
  | Executable_not_found of string      (** The requested executable does not exist. *)
  | Child_error of exit_status          (** The process exited with an error status. *)

type Exn.err += E of error

val err : error -> exn
(** [err e] is [Eio.Exn.create (E e)] *)

val pp_args : string list Fmt.t
(** Formats a list of arguments, quoting any that might cause confusion to the reader.

    This is intended for use in error messages and logging.*)

(** {2 Types} *)

type 'tag ty = [ `Process | `Platform of 'tag ]

type 'a t = ([> [> `Generic] ty] as 'a) r
(** A process. *)

type 'tag mgr_ty = [ `Process_mgr | `Platform of 'tag ]

type 'a mgr = 'a r
 constraint 'a = [> [> `Generic] mgr_ty]
(** A process manager capable of spawning new processes. *)

(** {2 Processes} *)

val pid : _ t -> int
(** [pid t] is the process ID of [t]. *)

val await : _ t  -> exit_status
(** [await t] waits for process [t] to exit and then reports the status. *)

val await_exn : ?is_success:(int -> bool) -> _ t  -> unit
(** Like {! await} except an exception is raised if does not return a successful
    exit status.

    @param is_success Used to determine if an exit code is successful.
                      Default is [Int.equal 0]. *)

val signal : _ t -> int -> unit
(** [signal t i] sends the signal [i] to process [t].

    If the process has already exited then this does nothing
    (it will not signal a different process, even if the PID has been reused).

    See {!Sys} for the signal numbers. *)

val spawn :
  sw:Switch.t ->
  [> 'tag mgr_ty] r ->
  ?cwd:Fs.dir_ty Path.t ->
  ?stdin:_ Flow.source ->
  ?stdout:_ Flow.sink ->
  ?stderr:_ Flow.sink ->
  ?env:string array ->
  ?executable:string ->
  string list -> 'tag ty r
(** [spawn ~sw mgr args] creates a new child process that is connected to the switch [sw].

    The child process will be sent {! Sys.sigkill} when the switch is released.

    If the flows [stdin], [stdout] and [stderr] are not backed by file descriptors then
    this also creates pipes and spawns fibers to copy the data as necessary.
    If you need more control over file descriptors, see {!Eio_unix.Process}.

    @param cwd The current working directory of the process (default: same as parent process).
    @param stdin The flow to attach to the process's standard input (default: same as parent process).
    @param stdout A flow that the process's standard output goes to (default: same as parent process).
    @param stderr A flow that the process's standard error goes to (default: same as parent process).
    @param env The environment for the process (default: same as parent process).
    @param executable The path of the executable to run.
                      If not given then the first item in [args] is used,
                      searching $PATH for it if necessary. *)

val run :
  _ mgr ->
  ?cwd:_ Path.t ->
  ?stdin:_ Flow.source ->
  ?stdout:_ Flow.sink ->
  ?stderr:_ Flow.sink ->
  ?is_success:(int -> bool) ->
  ?env:string array ->
  ?executable:string ->
  string list -> unit
(** [run] does {!spawn} followed by {!await_exn}, with the advantage that if the process fails then
    the error message includes the command that failed.

    When [is_success] is provided, it is called with the exit code to determine whether it indicates success or failure.
    Without [is_success], success requires the process to return an exit code of 0.

    Note: If [spawn] needed to create extra fibers to copy [stdin], etc, then it also waits for those to finish. *)

val parse_out :
  _ mgr ->
  'a Buf_read.parser ->
  ?cwd:_ Path.t ->
  ?stdin:_ Flow.source ->
  ?stderr:_ Flow.sink ->
  ?is_success:(int -> bool) ->
  ?env:string array ->
  ?executable:string ->
  string list -> 'a
(** [parse_out mgr parser args] runs [args] and parses the child's stdout with [parser].

    It also waits for the process to finish and checks its exit status is zero.

    Note that [parser] must consume the entire output of the process (like {!Buf_read.parse}).

    To return all the output as a string, use {!Buf_read.take_all} as the parser.

    This is a convenience wrapper around {!run},
    and the optional arguments have the same meanings. *)

(** {2 Pipes} *)

val pipe : sw:Switch.t -> _ mgr -> [Flow.source_ty | Resource.close_ty] r * [Flow.sink_ty | Resource.close_ty] r
(** [pipe ~sw mgr] creates a pipe backed by the OS.

    The flows can be used by {!spawn} without the need for extra fibers to copy the data.
    This can be used to connect multiple processes together. *)

(** {2 Provider Interface} *)
module Pi : sig
  module type PROCESS = sig
    type t
    type tag

    val pid : t -> int
    val await : t -> exit_status
    val signal : t -> int -> unit
  end

  type (_, _, _) Resource.pi +=
    | Process : ('t, (module PROCESS with type t = 't and type tag = 'tag), [> 'tag ty]) Resource.pi

  val process :
    (module PROCESS with type t = 't and type tag = 'tag) ->
    ('t, 'tag ty) Resource.handler

  module type MGR = sig
    type tag
    type t

    val pipe :
      t ->
      sw:Switch.t ->
      [Flow.source_ty | Resource.close_ty] r * [Flow.sink_ty | Resource.close_ty] r

    val spawn :
      t ->
      sw:Switch.t ->
      ?cwd:Fs.dir_ty Path.t ->
      ?stdin:Flow.source_ty r ->
      ?stdout:Flow.sink_ty r ->
      ?stderr:Flow.sink_ty r ->
      ?env:string array ->
      ?executable:string ->
      string list ->
      tag ty r
  end

  type (_, _, _) Resource.pi +=
    | Mgr : ('t, (module MGR with type t = 't and type tag = 'tag), [> 'tag mgr_ty]) Resource.pi

  val mgr :
    (module MGR with type t = 't and type tag = 'tag) ->
    ('t, 'tag mgr_ty) Resource.handler
end
