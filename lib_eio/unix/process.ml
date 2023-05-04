open Eio.Std

let resolve_program name =
  if Filename.is_implicit name then (
    Sys.getenv_opt "PATH"
    |> Option.value ~default:"/bin:/usr/bin"
    |> String.split_on_char ':'
    |> List.find_map (fun dir ->
        let p = Filename.concat dir name in
        if Sys.file_exists p then Some p else None
      )
  ) else if Sys.file_exists name then (
    Some name
  ) else None

let read_of_fd ~sw ~default ~to_close = function
  | None -> default
  | Some f ->
    match Resource.fd_opt f with
    | Some fd -> fd
    | None ->
      let r, w = Private.pipe sw in
      Fiber.fork ~sw (fun () ->
          Eio.Flow.copy f w;
          Eio.Flow.close w
        );
      let r = Resource.fd r in
      to_close := r :: !to_close;
      r

let write_of_fd ~sw ~default ~to_close = function
  | None -> default
  | Some f ->
    match Resource.fd_opt f with
    | Some fd -> fd
    | None ->
      let r, w = Private.pipe sw in
      Fiber.fork ~sw (fun () ->
          Eio.Flow.copy r f;
          Eio.Flow.close r
        );
      let w = Resource.fd w in
      to_close := w :: !to_close;
      w

let with_close_list fn =
  let to_close = ref [] in
  let close () =
    List.iter Fd.close !to_close
  in
  match fn to_close with
  | x -> close (); x
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    close ();
    Printexc.raise_with_backtrace ex bt

let get_executable ~args = function
  | Some exe -> exe
  | None ->
    match args with
    | [] -> invalid_arg "Arguments list is empty and no executable given!"
    | (x :: _) ->
      match resolve_program x with
      | Some x -> x
      | None -> raise (Eio.Process.err (Executable_not_found x))

let get_env = function
  | Some e -> e
  | None -> Unix.environment ()

class virtual mgr = object (self)
  inherit Eio.Process.mgr

  method pipe ~sw = (Private.pipe sw :> <Eio.Flow.source; Eio.Flow.close> * <Eio.Flow.sink; Eio.Flow.close>)

  method virtual spawn_unix :
    sw:Switch.t ->
    ?cwd:Eio.Fs.dir Eio.Path.t ->
    env:string array ->
    fds:(int * Fd.t * Fork_action.blocking) list ->
    executable:string ->
    string list ->
    Eio.Process.t

  method spawn ~sw ?cwd ?stdin ?stdout ?stderr ?env ?executable args =
    let executable = get_executable executable ~args in
    let env = get_env env in
    with_close_list @@ fun to_close ->
    let stdin_fd  = read_of_fd  ~sw stdin  ~default:Fd.stdin  ~to_close in
    let stdout_fd = write_of_fd ~sw stdout ~default:Fd.stdout ~to_close in
    let stderr_fd = write_of_fd ~sw stderr ~default:Fd.stderr ~to_close in
    let fds = [
      0, stdin_fd, `Blocking;
      1, stdout_fd, `Blocking;
      2, stderr_fd, `Blocking;
    ] in
    self#spawn_unix ~sw ?cwd ~env ~fds ~executable args
end

let spawn_unix ~sw (mgr:#mgr) ?cwd ~fds ?env ?executable args =
  let executable = get_executable executable ~args in
  let env = get_env env in
  mgr#spawn_unix ~sw ?cwd ~fds ~env ~executable args
