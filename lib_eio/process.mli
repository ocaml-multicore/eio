type status = Exited of int | Signaled of int | Stopped of int

val pp_status : Format.formatter -> status -> unit

class virtual t : object
    method virtual pid : int
    method virtual exit_status : status
    method virtual signal : int -> unit
end

val pid : #t -> int
(** The process ID *)

val exit_status : #t -> status
(** Reports the exit status of the subprocess. This will block waiting for the subprocess
    to exit. *)

val signal : #t -> int -> unit
(** [signal t i] sends the signal [i] to process [t]. *)

class virtual mgr : object
    method virtual spawn : 'a 'b 'c.
        sw:Switch.t ->
        ?cwd:Fs.dir Path.t ->
        stdin:(#Flow.source as 'a) ->
        stdout:(#Flow.sink as 'b) ->
        stderr:(#Flow.sink as 'c) ->
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
