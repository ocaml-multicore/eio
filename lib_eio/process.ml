type exit_status = [
  | `Exited of int
  | `Signaled of int
]

type status = [ exit_status | `Stopped of int ]

let pp_status ppf = function
  | `Exited i -> Format.fprintf ppf "Exited (code %i)" i
  | `Signaled i -> Format.fprintf ppf "Exited (signal %a)" Fmt.Dump.signal i
  | `Stopped i -> Format.fprintf ppf "Stopped (signal %a)" Fmt.Dump.signal i

type error =
  | Executable_not_found of string
  | Child_error of exit_status

type Exn.err += E of error

let err e = Exn.create (E e)

let () =
  Exn.register_pp (fun f -> function
    | E e ->
      Fmt.string f "Process ";
      begin match e with
        | Executable_not_found e -> Fmt.pf f "Executable %S not found" e;
        | Child_error e -> Fmt.pf f "Child_error %a" pp_status e;
      end;
      true
    | _ -> false
  )

class virtual t = object
  method virtual pid : int
  method virtual await : exit_status
  method virtual signal : int -> unit
end

let pid proc = proc#pid
let await proc = proc#await

let await_exn proc =
  match proc#await with
  | `Exited 0 -> ()
  | status -> raise (err (Child_error status))

let signal proc = proc#signal

class virtual mgr = object
  method virtual pipe :
    sw:Switch.t ->
    <Flow.source; Flow.close> * <Flow.sink; Flow.close>

  method virtual spawn :
    sw:Switch.t ->
    ?cwd:Fs.dir Path.t ->
    ?stdin:Flow.source ->
    ?stdout:Flow.sink ->
    ?stderr:Flow.sink ->
    ?env:string array ->
    ?executable:string ->
    string list ->
    t
end

let bad_char = function
  | ' ' | '"' | '\'' | '\\' -> true
  | c ->
    let c = Char.code c in
    c <= 32 || c >= 127

let pp_arg f x =
  if x = "" || String.exists bad_char x then Fmt.(quote string) f x
  else Fmt.string f x

let pp_args = Fmt.hbox (Fmt.list ~sep:Fmt.sp pp_arg)

let spawn ~sw (t:#mgr) ?cwd ?stdin ?stdout ?stderr ?env ?executable args =
  t#spawn ~sw
    ?cwd:(cwd :> Fs.dir Path.t option)
    ?env
    ?executable args
    ?stdin:(stdin :> Flow.source option)
    ?stdout:(stdout :> Flow.sink option)
    ?stderr:(stderr :> Flow.sink option)

let run (t:#mgr) ?cwd ?stdin ?stdout ?stderr ?env ?executable args =
  Switch.run @@ fun sw ->
  let child = spawn ~sw t ?cwd ?stdin ?stdout ?stderr ?env ?executable args in
  match await child with
  | `Exited 0 -> ()
  | status ->
    let ex = err (Child_error status) in
    raise (Exn.add_context ex "running command: %a" pp_args args)

let pipe ~sw (t:#mgr) = t#pipe ~sw

let parse_out (t:#mgr) parse ?cwd ?stdin ?stderr ?env ?executable args =
  Switch.run @@ fun sw ->
  let r, w = pipe t ~sw in
  try
    let child = spawn ~sw t ?cwd ?stdin ~stdout:w ?stderr ?env ?executable args in
    Flow.close w;
    let output = Buf_read.parse_exn parse r ~max_size:max_int in
    Flow.close r;
    await_exn child;
    output
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "running command: %a" pp_args args
