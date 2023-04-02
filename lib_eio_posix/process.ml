open Low_level
module Rfcd = Eio_unix.Private.Rcfd

let unix_status_to_stats = function
  | Unix.WEXITED i -> Eio.Process.Exited i
  | Unix.WSIGNALED i -> Eio.Process.Signaled i
  | Unix.WSTOPPED i -> Eio.Process.Stopped i

let process proc : Eio.Process.t = object
  method pid = Process.pid proc
  method await =
    let status = Eio.Promise.await @@ Process.exit_status proc in
    unix_status_to_stats status
  method signal i = Process.signal proc i
end

let read_of_fd ~sw flow =
  match Fd.get_fd_opt flow with
  | None ->
    let r, w = pipe ~sw in
    Some (Flow.of_fd w), r
  | Some fd -> None, fd

let write_of_fd ~sw t =
  match Fd.get_fd_opt t with
  | None ->
    let r, w = pipe ~sw in
    Some (Flow.of_fd r), w
  | Some fd -> None, fd

let v = object
  inherit Eio.Process.mgr

  method spawn ~sw ?cwd ~(stdin : #Eio.Flow.source) ~(stdout : #Eio.Flow.sink) ~(stderr : #Eio.Flow.sink) prog args =
    let stdin_w, stdin_fd = read_of_fd ~sw stdin in
    let stdout_r, stdout_fd = write_of_fd ~sw stdout in
    let stderr_r, stderr_fd = write_of_fd ~sw stderr in
    let actions = Process.Fork_action.[
      Eio_unix.Private.Fork_action.inherit_fds [
        0, Fd.to_rcfd stdin_fd, `Blocking;
        1, Fd.to_rcfd stdout_fd, `Blocking;
        2, Fd.to_rcfd stderr_fd, `Blocking;
      ];
      execve prog ~argv:(Array.of_list args) ~env:[||]
    ] in
    let with_actions cwd fn = match cwd with
      | Some ((dir, path) : Eio.Fs.dir Eio.Path.t) -> (
        match Eio.Generic.probe dir Fs.Posix_dir with
        | None -> fn actions
        | Some posix ->
          posix#with_parent_dir path @@ fun dirfd s ->
          let cwd = Low_level.openat ?dirfd ~sw ~mode:0 s Low_level.Open_flags.(rdonly + directory) in
          fn (Process.Fork_action.fchdir cwd :: actions)
      )
      | None -> fn actions
    in
    let proc =
      with_actions cwd @@ fun actions ->
      process (Process.spawn ~sw actions)
    in
    Option.iter (fun stdin_w ->
      Eio.Flow.copy stdin stdin_w;
      Eio.Flow.close stdin_w
    ) stdin_w;
    Option.iter (fun stdout_r ->
      Fd.close stdout_fd;
      Eio.Flow.copy stdout_r stdout) stdout_r;
    Option.iter (fun stderr_r ->
      Fd.close stderr_fd;
      Eio.Flow.copy stderr_r stdout) stderr_r;
    proc
end