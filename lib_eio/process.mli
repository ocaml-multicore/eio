type status = Exited of int | Signaled of int | Stopped of int

val pp_status : Format.formatter -> status -> unit

class virtual process : object
    inherit Generic.t
    method virtual pid : int
    method virtual status : status
    method virtual stop : unit
end

val pid : #process -> int
(** The process ID *)

val status : #process -> status
(** Checks the status of the subprocess, this will block waiting for the subprocess
    to terminate or be stopped by a signal. *)

val stop : #process -> unit
(** Stop a running subprocess *)

class virtual t : object
    inherit Generic.t
    method virtual spawn : 
        sw:Switch.t -> 
        ?cwd:string ->
        ?stderr:Flow.sink ->
        ?stdout:Flow.sink -> 
        ?stdin:Flow.source ->
        string ->
        string list ->
        process
    
    method virtual spawn_detached :
        ?cwd:string ->
        ?stderr:Flow.sink ->
        ?stdout:Flow.sink -> 
        ?stdin:Flow.source ->
        string ->
        string list ->
        process
end

val spawn : sw:Switch.t -> #t -> ?cwd:string -> ?stderr:Flow.sink -> ?stdout:Flow.sink -> ?stdin:Flow.source -> string -> string list -> process
(** [spawn ~sw t cmd] creates a new subprocess that is connected to the
    switch [sw]. The standard input and outputs redirect to nothing by default. *)

val spawn_detached : #t -> ?cwd:string -> ?stderr:Flow.sink -> ?stdout:Flow.sink -> ?stdin:Flow.source -> string -> string list -> process
(** [spawn_detached t cmd] is like {! spawn} but the new subprocess is not
    attached to any switch. *)