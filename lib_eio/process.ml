type status = Exited of int | Signaled of int | Stopped of int

let pp_status ppf = function
  | Exited i -> Format.fprintf ppf "Exited %i" i
  | Signaled i -> Format.fprintf ppf "Signalled %i" i
  | Stopped i -> Format.fprintf ppf "Stopped %i" i

type Exn.err += E of status

let err e = Exn.create (E e)

let () =
  Exn.register_pp (fun f -> function
    | E e ->
      Fmt.string f "Process ";
      pp_status f e;
      true
    | _ -> false
  )

class virtual t = object
  method virtual pid : int
  method virtual await : status
  method virtual signal : int -> unit
end

let pid proc = proc#pid
let await proc = proc#await

let await_exn proc =
  match proc#await with
  | Exited 0 -> ()
  | status -> raise (err status)

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