module C = Configurator.V1

let mangle (s:string) =
  let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false in
  let rec first_alpha i =
    if is_alpha (String.get s i) then
      i
    else
      first_alpha (succ i)
  in
  let skip = first_alpha 0 in
  String.sub s skip ((String.length s) - skip) |>
  String.lowercase_ascii

let () =
  let open C.C_define in
  C.main ~name:"discover" @@
  fun c ->
  let sig_build =
    C.C_define.import c
      ~c_flags:["-I"; Filename.concat (Sys.getcwd ()) "include"]
      ~includes:["signal.h"]
  in
  let process =
    List.map
      (function
        | name, Value.Int v ->
          Printf.sprintf "  let %s = %d" (mangle name) v
        | name, Value.Switch v ->
          if not v then
            Printf.sprintf "  let %s_opt = None" (mangle name)
          else
            (match sig_build Type.[ name, Int; ] with
             | (name, Value.Int v) :: [] ->
               Printf.sprintf "  let %s_opt = Some %d" (mangle name) v
             | _ -> assert false)
        | _ -> assert false)
  in
  let signums =
    sig_build
      Type.[
        "SIGHUP", Int;
        "SIGINT", Int;
        "SIGQUIT", Int;
        "SIGILL", Int;
        "SIGTRAP", Int;
        "SIGABRT", Int;
        "SIGEMT", Switch;
        "SIGFPE", Int;
        "SIGKILL", Int;
        "SIGBUS", Int;
        "SIGSEGV", Int;
        "SIGSYS", Int;
        "SIGPIPE", Int;
        "SIGALRM", Int;
        "SIGTERM", Int;
        "SIGURG", Int;
        "SIGSTOP", Int;
        "SIGTSTP", Int;
        "SIGCONT", Int;
        "SIGCHLD", Int;
        "SIGTTIN", Int;
        "SIGTTOU", Int;
        "SIGIO", Int;
        "SIGXCPU", Int;
        "SIGXFSZ", Int;
        "SIGVTALRM", Int;
        "SIGPROF", Int;
        "SIGWINCH", Int;
        "SIGINFO", Switch;
        "SIGUSR1", Int;
        "SIGUSR2", Int;
        "SIGTHR", Switch;
      ] |> process
  in
  let nsig =
    let maxfound =
      sig_build
        Type.[
          "NSIG", Switch;
          "_NSIG", Switch;
          "_SIG_MAXSIG", Switch;
        ] |>
      List.map
        (function
          | name, Value.Switch v ->
            if not v then
              0
            else
              (match sig_build Type.[ name, Int; ] with
               | (_, Value.Int v) :: [] -> v
               | _ -> assert false)
          | _ -> assert false)
    in
    let maxfound = List.fold_left max 64 maxfound in
    [ Printf.sprintf "  let nsig = %d" maxfound ]
  in
  C.Flags.write_lines "config.ml" @@
  [ "module Signum = struct" ] @
  nsig @
  signums @
  [ "end" ]
