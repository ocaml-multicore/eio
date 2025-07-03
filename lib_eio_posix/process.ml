open Eio.Std

module Process_impl = struct
  type t = Low_level.Process.t
  type tag = [ `Generic | `Unix ]

  let pid = Low_level.Process.pid

  let await t =
    match Eio.Promise.await @@ Low_level.Process.exit_status t with
    | Unix.WEXITED i -> `Exited i
    | Unix.WSIGNALED i -> `Signaled i
    | Unix.WSTOPPED _ -> assert false

  let signal = Low_level.Process.signal
end

let process =
  let handler = Eio.Process.Pi.process (module Process_impl) in
  fun proc -> Eio.Resource.T (proc, handler)

module Impl = struct
  module T = struct
    type t = unit

    let spawn_unix () ~sw ?cwd ~env ~fds ~executable args =
      let actions = Low_level.Process.Fork_action.[
          inherit_fds fds;
          execve executable ~argv:(Array.of_list args) ~env
      ] in
      let with_actions cwd fn = match cwd with
        | None -> fn actions
        | Some ((dir, path) : Eio.Fs.dir_ty Eio.Path.t) ->
          match Fs.as_posix_dir dir with
          | None -> Fmt.invalid_arg "cwd is not an OS directory!"
          | Some dirfd ->
            Switch.run ~name:"spawn_unix" @@ fun launch_sw ->
            let cwd = Err.run (fun () ->
              let flags = Low_level.Open_flags.(rdonly + directory) in
              Low_level.openat ~sw:launch_sw ~mode:0 dirfd path flags) () in
            fn (Low_level.Process.Fork_action.fchdir cwd :: actions)
      in
      with_actions cwd @@ fun actions ->
      process (Low_level.Process.spawn ~sw actions)
  end

  include Eio_unix.Process.Make_mgr (T)
end

let mgr : Eio_unix.Process.mgr_ty r =
  let h = Eio_unix.Process.Pi.mgr_unix (module Impl) in
  Eio.Resource.T ((), h)
