open Std

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
  | Argument_list_too_long
  | Permission_denied of string
  | Executable_format_error of string

type Exn.err += E of error

let err e = Exn.create (E e)

let () =
  Exn.register_pp (fun f -> function
    | E e ->
      Fmt.string f "Process ";
      begin match e with
        | Executable_not_found e -> Fmt.pf f "Executable_not_found %S" e;
        | Child_error e -> Fmt.pf f "Child_error %a" pp_status e;
        | Argument_list_too_long -> Fmt.pf f "Argument_list_too_long"
        | Permission_denied e -> Fmt.pf f "Permission_denied %S" e
        | Executable_format_error e -> Fmt.pf f "Executable_format_error %S" e
      end;
      true
    | _ -> false
  )

type 'tag ty = [ `Process | `Platform of 'tag ]

type 'a t = ([> [> `Generic] ty] as 'a) r

type 'tag mgr_ty = [ `Process_mgr | `Platform of 'tag ]

type 'a mgr = 'a r
 constraint 'a = [> [> `Generic] mgr_ty]

module Env = struct
  let on_windows = (Sys.os_type = "Win32")

  module Name = struct
    type t = string

    let normalise =
      if on_windows then String.uppercase_ascii
      else Fun.id

    let compare x y =
      String.compare (normalise x) (normalise y)

    let starts_with ~prefix =
      let prefix = normalise prefix in
      fun x -> String.starts_with ~prefix (normalise x)

    let validate t =
      let bad_char = function
        | '\000' | '=' -> true
        | _ -> false
      in
      if t = "" || String.exists bad_char t then
        Fmt.invalid_arg "Invalid environment variable name %S" t
  end

  module M = Map.Make(Name)

  type t = string array

  let of_array = Fun.id
  let to_array = Fun.id
  let empty = [| |]

  let validate_value value =
    if String.contains value '\000' then
      Fmt.invalid_arg "Invalid environment variable value %S" value

  let validate_binding (name, value) =
    Name.validate name;
    Option.iter validate_value value

  let entry name value =
    Printf.sprintf "%s=%s" name value

  let get_opt name t =
    Name.validate name;
    let prefix = name ^ "=" in
    Array.find_opt (Name.starts_with ~prefix) t
    |> Option.map (fun e ->
        let i = String.length prefix in
        String.sub e i (String.length e - i)
      )

  let override bindings t =
    List.iter validate_binding bindings;
    let all_bindings = M.of_list bindings in
    let bindings = ref all_bindings in
    let updated =
      Array.to_list t
      |> List.filter_map (fun e ->
          match String.index e '=' with
          | exception Not_found -> Some e       (* Not a normal k=v entry *)
          | i ->
            let name = String.sub e 0 i in
            match M.find_opt name all_bindings with
            | None -> Some e                    (* We're not changing this *)
            | Some x -> 
              if M.mem name !bindings then (
                bindings := M.remove name !bindings;
                match x with
                | None -> None                    (* Remove existing entry *)
                | Some v -> Some (entry name v)   (* Update existing entry *)
              ) else None                         (* Remove duplicate entry *)
        )
    in
    let extra =
      M.to_list !bindings
      |> List.filter_map (function
          | _, None -> None                     (* Remove entry that wasn't there anyway *)
          | k, Some v ->
            Some (entry k v)                    (* Add new entry *)
        )
    in
    Array.of_list (updated @ extra)

  let of_bindings xs =
    override (List.map (fun (k, v) -> (k, Some v)) xs) empty

  let pp f t =
    Fmt.pf f "[@[<v>%a@]]"
      (Fmt.array ~sep:Fmt.cut (Fmt.fmt "%S")) t
end

module Pi = struct
  module type PROCESS = sig
    type t
    type tag

    val pid : t -> int
    val await : t -> exit_status
    val signal : t -> int -> unit
  end

  type (_, _, _) Resource.pi +=
    | Process : ('t, (module PROCESS with type t = 't and type tag = 'tag), [> 'tag ty]) Resource.pi

  let process (type t tag) (module X : PROCESS with type t = t and type tag = tag) =
    Resource.handler [
      H (Process, (module X));
    ]

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

  let mgr (type t tag) (module X : MGR with type t = t and type tag = tag) =
    Resource.handler [
      H (Mgr, (module X));
    ]
end

let bad_char = function
  | ' ' | '"' | '\'' | '\\' -> true
  | c ->
    let c = Char.code c in
    c <= 32 || c >= 127

let pp_arg f x =
  if x = "" || String.exists bad_char x then Fmt.pf f "%S" x
  else Fmt.string f x

let pp_args = Fmt.hbox (Fmt.list ~sep:Fmt.sp pp_arg)

let await (type tag) ((Resource.T (v, ops)) : [> tag ty] r) =
  let module X = (val (Resource.get ops Pi.Process)) in
  X.await v

let await_exn ?(is_success = Int.equal 0) proc =
  match await proc with
  | `Exited code when is_success code -> ()
  | status -> raise (err (Child_error status))

let pid (type tag) (t : [> tag ty] r) =
  let (Resource.T (v, ops)) = t in
  let module X = (val (Resource.get ops Pi.Process)) in
  X.pid v

let signal (type tag) (t : [> tag ty] r) s =
  let (Resource.T (v, ops)) = t in
  let module X = (val (Resource.get ops Pi.Process)) in
  X.signal v s

let spawn (type tag) ~sw (t : [> tag mgr_ty] r) ?cwd ?stdin ?stdout ?stderr ?env ?executable args : tag ty r =
  let (Resource.T (v, ops)) = t in
  let module X = (val (Resource.get ops Pi.Mgr)) in
  X.spawn v ~sw
    ?cwd:(cwd :> Fs.dir_ty Path.t option)
    ?env
    ?executable args
    ?stdin:(stdin :> Flow.source_ty r option)
    ?stdout:(stdout :> Flow.sink_ty r option)
    ?stderr:(stderr :> Flow.sink_ty r option)

let run t ?cwd ?stdin ?stdout ?stderr ?(is_success = Int.equal 0) ?env ?executable args =
  Switch.run ~name:"Process.run" @@ fun sw ->
  let child = spawn ~sw t ?cwd ?stdin ?stdout ?stderr ?env ?executable args in
  match await child with
  | `Exited code when is_success code -> ()
  | status ->
    let ex = err (Child_error status) in
    raise (Exn.add_context ex "running command: %a" pp_args args)

let pipe (type tag) ~sw ((Resource.T (v, ops)) : [> tag mgr_ty] r) =
  let module X = (val (Resource.get ops Pi.Mgr)) in
  let r, w = X.pipe v ~sw in
  let r = (r : [Flow.source_ty | Resource.close_ty] r :> [< Flow.source_ty | Resource.close_ty] r) in
  let w = (w : [Flow.sink_ty   | Resource.close_ty] r :> [< Flow.sink_ty   | Resource.close_ty] r) in
  r, w

let parse_out (type tag) (t : [> tag mgr_ty] r) parse ?cwd ?stdin ?stderr ?is_success ?env ?executable args =
  Switch.run ~name:"Process.parse_out" @@ fun sw ->
  let r, w = pipe t ~sw in
  try
    let child = spawn ~sw t ?cwd ?stdin ~stdout:w ?stderr ?env ?executable args in
    Flow.close w;
    let output = Buf_read.parse_exn parse r ~max_size:max_int in
    Flow.close r;
    await_exn ?is_success child;
    output
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "running command: %a" pp_args args
