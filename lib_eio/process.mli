type status = Exited of int | Signaled of int | Stopped of int

val pp_status : Format.formatter -> status -> unit

type Exn.err += E of status

val err : status -> exn
(** [err e] is [Eio.Exn.create (E e)] *)

class virtual t : object
    method virtual pid : int
    method virtual await : status
    method virtual signal : int -> unit
end

val pid : #t -> int
(** The process ID *)

val await : #t -> status
(** This functions waits for the subprocess to exit and then reports the status. *)

val await_exn : #t -> unit
(** Like {! await} except an exception is raised if the status is not [Exited 0]. *)

val signal : #t -> int -> unit
(** [signal t i] sends the signal [i] to process [t]. *)

class virtual mgr : object
    method virtual spawn : 
        sw:Switch.t ->
        ?cwd:Fs.dir Path.t ->
        stdin:Flow.source ->
        stdout:Flow.sink ->
        stderr:Flow.sink ->
        string ->
        string list ->
        t
end
(** A process manager capable of spawning new processes. *)

val spawn : sw:Switch.t -> #mgr -> ?cwd:Fs.dir Path.t -> stdin:#Flow.source -> stdout:#Flow.sink -> stderr:#Flow.sink -> string -> string list -> t
(** [spawn ~sw mgr ?cwd ~stdin ~stdout ~stderr cmd args] creates a new subprocess that is connected to the
    switch [sw]. A process will be sent {! Sys.sigkill} when the switch is released.
    
    You must provide a standard input and outputs that are backed by file descriptors and
    [cwd] will optionally change the current working directory of the process.*)
