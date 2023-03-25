open Eio.Std

module Process = Low_level.Process

let process proc : Eio.Process.t = object
  method pid = Process.pid proc

  method await = 
    match Eio.Promise.await @@ Process.exit_status proc with
    | Unix.WEXITED i -> `Exited i
    | Unix.WSIGNALED i -> `Signaled i
    | Unix.WSTOPPED _ -> assert false

  method signal i = Process.signal proc i
end

let v = object
  inherit Eio_unix.Process.mgr

  method spawn_unix ~sw ?cwd ~env ~fds ~executable args =
    let actions = Process.Fork_action.[
        inherit_fds fds;
        execve executable ~argv:(Array.of_list args) ~env
    ] in
    let with_actions cwd fn = match cwd with
      | None -> fn actions
      | Some ((dir, path) : Eio.Fs.dir Eio.Path.t) ->
        match Eio.Generic.probe dir Fs.Posix_dir with
        | None -> Fmt.invalid_arg "cwd is not an OS directory!"
        | Some posix ->
          posix#with_parent_dir path @@ fun dirfd s ->
          Switch.run @@ fun launch_sw ->
          let cwd = Low_level.openat ?dirfd ~sw:launch_sw ~mode:0 s Low_level.Open_flags.(rdonly + directory) in
          fn (Process.Fork_action.fchdir cwd :: actions)
    in
    with_actions cwd @@ fun actions -> 
    process (Process.spawn ~sw actions)
end
