type status = Exited of int | Signaled of int | Stopped of int

let pp_status ppf = function
  | Exited i -> Format.fprintf ppf "Exited %i" i
  | Signaled i -> Format.fprintf ppf "Signalled %i" i
  | Stopped i -> Format.fprintf ppf "Stopped %i" i

class virtual t = object (_ : #Generic.t)
  method probe _ = None
  method virtual pid : int
  method virtual status : status
  method virtual stop : unit
end

let pid proc = proc#pid
let status proc = proc#status
let stop proc = proc#stop

class virtual mgr = object (_ : #Generic.t)
  method probe _ = None
  method virtual spawn : sw:Switch.t -> ?cwd:string -> ?stderr:Flow.sink -> ?stdout:Flow.sink -> ?stdin:Flow.source -> string -> string list -> t
  method virtual spawn_detached : ?cwd:string -> ?stderr:Flow.sink -> ?stdout:Flow.sink -> ?stdin:Flow.source -> string -> string list -> t
end

let spawn ~sw t ?cwd ?stderr ?stdout ?stdin cmd args = t#spawn ~sw ?cwd ?stderr ?stdout ?stdin  cmd args
let spawn_detached t ?cwd ?stderr ?stdout ?stdin cmd args = t#spawn_detached ?cwd ?stderr ?stdout ?stdin cmd args