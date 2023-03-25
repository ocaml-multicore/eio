type status = Exited of int | Signaled of int | Stopped of int

let pp_status ppf = function
  | Exited i -> Format.fprintf ppf "Exited %i" i
  | Signaled i -> Format.fprintf ppf "Signalled %i" i
  | Stopped i -> Format.fprintf ppf "Stopped %i" i

class virtual t = object
  method virtual pid : int
  method virtual status : status
  method virtual signal : int -> unit
end

let pid proc = proc#pid
let status proc = proc#status
let signal proc = proc#signal

class virtual mgr = object
  method virtual spawn : sw:Switch.t -> ?cwd:Fs.dir Path.t -> stdin:Flow.source -> stdout:Flow.sink -> stderr:Flow.sink -> string -> string list -> t
end

let spawn ~sw t ?cwd ~stdin ~stdout ~stderr cmd args = t#spawn ~sw ?cwd ~stdin ~stdout ~stderr cmd args