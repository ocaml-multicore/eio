type status = Exited of int | Signaled of int | Stopped of int

let pp_status ppf = function
  | Exited i -> Format.fprintf ppf "Exited %i" i
  | Signaled i -> Format.fprintf ppf "Signalled %i" i
  | Stopped i -> Format.fprintf ppf "Stopped %i" i

class virtual t = object
  method virtual pid : int
  method virtual exit_status : status
  method virtual signal : int -> unit
end

let pid proc = proc#pid
let exit_status proc = proc#exit_status
let signal proc = proc#signal

class virtual mgr = object
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

let spawn ~sw (t:#mgr) ?cwd ~stdin ~stdout ~stderr cmd args = 
  t#spawn ~sw ?cwd cmd args
    ~stdin:(stdin :> Flow.source)
    ~stdout:(stdout :> Flow.sink)
    ~stderr:(stderr :> Flow.sink)