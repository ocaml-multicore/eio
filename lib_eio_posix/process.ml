open Low_level
module Rfcd = Eio_unix.Private.Rcfd

let unix_status_to_stats = function
  | Unix.WEXITED i -> Eio.Process.Exited i
  | Unix.WSIGNALED i -> Eio.Process.Signaled i
  | Unix.WSTOPPED i -> Eio.Process.Stopped i

let process proc : Eio.Process.t = object
  method pid = Process.pid proc
  method status = 
    let status = Eio.Promise.await @@ Process.exit_status proc in
    unix_status_to_stats status
  method signal i = Process.signal proc i
end

let pipe_or_fd flow =
  match Fd.get_fd_opt flow with
  | None -> assert false
  | Some fd -> Fd.to_rcfd fd

let v = object
  method spawn ~sw ?cwd ~stdin ~stdout ~stderr prog args = 
    let chdir = Option.to_list cwd |> List.map (fun (_, s) -> Process.Fork_action.chdir s) in
    let stdin = pipe_or_fd stdin in
    let stdout = pipe_or_fd stdout in
    let stderr = pipe_or_fd stderr in
    let actions = Process.Fork_action.[
      Eio_unix.Private.Fork_action.inherit_fds [
        0, stdin, `Blocking;
        1, stdout, `Blocking;
        2, stderr, `Blocking;
      ];
      execve prog ~argv:(Array.of_list args) ~env:[||]
    ] in
    let actions = chdir @ actions in 
    process (Process.spawn ~sw actions)
end