open Eio.Std

module Fd = Eio_unix.Fd

external eio_spawn :
  string option -> string ->
  Unix.file_descr -> Unix.file_descr -> Unix.file_descr ->
  string -> int * Unix.file_descr
  = "caml_eio_windows_spawn_bytes" "caml_eio_windows_spawn"

external eio_process_wait : Unix.file_descr -> int = "caml_eio_windows_process_wait"
external eio_process_terminate : Unix.file_descr -> int -> bool = "caml_eio_windows_process_terminate"

(* CreateProcess expects entry to be NUL-terminated with a final NUL. *)
let env_block env =
  Array.iter
    (fun s ->
       if s = "" || String.contains s '\000' then
         Fmt.invalid_arg "spawn: invalid environment entry %S" s)
    env;
  String.concat "\000" (Array.to_list env) ^ "\000\000"

(* Act like the OCaml stdlib and only quote where needed, as cmd.exe treats
   quoted arguments differently even when don't contain any spaces. *)
let command_line args =
  let quote_arg arg =
    if arg = "" || String.exists (function ' ' | '\t' | '\n' | '\011' | '"' -> true | _ -> false) arg
    then Filename.quote arg
    else arg in
  String.concat " " (List.map quote_arg args)

let terminated_exit_code = Config.status_control_c_exit

module Process = struct
  type t = {
    pid : int;
    handle : Fd.t;
    mutable signalled : int option;
    exited : (int, exn) result Promise.t;
  }
  type tag = [ `Generic | `Unix ]

  let pid t = t.pid

  let await t =
    let code = Promise.await_exn t.exited in
    match t.signalled with
    | Some signum when code = terminated_exit_code -> `Signaled signum
    | _ -> `Exited code

  (* Windows has no signals, so any signal terminates the process. *)
  let signal t signum =
    if not (Promise.is_resolved t.exited) then
      Fd.use t.handle ~if_closed:ignore (fun h ->
          if eio_process_terminate h terminated_exit_code then t.signalled <- Some signum)

  let wait t =
    Fd.use_exn "process_wait" t.handle @@ fun h ->
    Eio_unix.run_in_systhread ~label:"process_wait" (fun () -> eio_process_wait h)

  let start ~sw create =
    let started, set_started = Promise.create () in
    Fiber.check ();
    Switch.check sw;
    Fiber.fork_daemon ~sw (fun () ->
        Fiber.check ();
        match create () with
        | exception ex -> Promise.resolve_error set_started ex; `Stop_daemon
        | pid, raw_handle ->
          match Switch.run (fun sw ->
              let handle = Fd.of_unix ~sw ~blocking:true ~close_unix:false raw_handle in
              let exited, set_exited = Promise.create () in
              let t = { pid; handle; signalled = None; exited } in
              Fiber.fork_daemon ~sw (fun () ->
                  try Fiber.await_cancel ()
                  with ex -> signal t Sys.sigkill; raise ex);
              Promise.resolve_ok set_started t;
              Eio.Cancel.protect (fun () ->
                  Promise.resolve set_exited (try Ok (wait t) with ex -> Error ex))) with
          | () -> Unix.close raw_handle; `Stop_daemon
          | exception ex ->
            ignore (eio_process_terminate raw_handle terminated_exit_code);
            Unix.close raw_handle;
            raise ex);
    Promise.await_exn started

end

let process_handler = Eio.Process.Pi.process (module Process)
let process t = Eio.Resource.T (t, process_handler)

module Impl = struct
  let stdio_compatible raw =
    match File_io.kind raw with
    | File_io.Pollable | File_io.Synchronous -> true
    | File_io.File | File_io.Append_file | File_io.Pipe | File_io.Socket -> false

  module T = struct
    type t = unit

    (* [realpath] gives a verbatim (\\?\) path. CreateProcess accepts that, but
       programs such as cmd.exe can't use it as their current directory. *)
    let cwd_path ((dir, path) : Eio.Fs.dir_ty Eio.Path.t) =
      match Fs.Handler.as_posix_dir dir with
      | None -> Fmt.invalid_arg "cwd is not an eio_windows directory!"
      | Some d ->
        Eio_utils.Nt_path.to_win32 (Err.run Low_level.realpath (Fs.Dir.resolve d path))

    let spawn_unix () ~sw ?cwd ?pgid ?uid ?gid ?login_tty ~env ~fds ~executable args =
      if pgid <> None || uid <> None || gid <> None then
        Fmt.invalid_arg "spawn: pgid/uid/gid are not supported on Windows";
      if login_tty <> None then
        Fmt.invalid_arg "spawn: login_tty is not supported on Windows";
      Switch.check sw;
      List.iter (fun (i, _, _) ->
          if i > 2 then Fmt.invalid_arg "spawn: only fds 0-2 are supported on Windows (got fd %d)" i)
        fds;
      let cmdline =
        command_line (executable :: (match args with [] -> [] | _ :: tl -> tl))
      in
      let env = env_block env in
      (* Anything not listed is inherited, as on POSIX. *)
      let get n default =
        Option.value ~default (List.find_map (fun (i, fd, _) -> if i = n then Some fd else None) fds)
      in
      let stdin_fd = get 0 Fd.stdin and stdout_fd = get 1 Fd.stdout and stderr_fd = get 2 Fd.stderr in
      let cwd = Option.map cwd_path cwd in
      Fd.use_exn "stdin" stdin_fd @@ fun h0 ->
      Fd.use_exn "stdout" stdout_fd @@ fun h1 ->
      Fd.use_exn "stderr" stderr_fd @@ fun h2 ->
      if not (stdio_compatible h0 && stdio_compatible h1 && stdio_compatible h2) then
        invalid_arg "spawn_unix: standard handles must support synchronous IO";
      process (Process.start ~sw (fun () -> eio_spawn cwd env h0 h1 h2 cmdline))
  end

  include Eio_unix.Process.Make_mgr (T)

  let pipe () ~sw =
    Switch.check sw;
    let r, w = Unix.pipe ~cloexec:true () in
    let wrap fd = Flow.of_fd ~kind:File_io.Pollable (Fd.of_unix ~sw ~blocking:true ~close_unix:true fd) in
    wrap r, wrap w

  let copy_stdin source sink =
    let buf = Cstruct.create 4096 in
    let rec copy () =
      match Eio.Flow.single_read source buf with
      | exception End_of_file -> ()
      | n ->
        match Eio.Flow.write sink [Cstruct.sub buf 0 n] with
        | () -> copy ()
        | exception Eio.Io (Eio.Net.E (Connection_reset _), _) -> ()
    in
    copy ()

  let spawn v ~sw ?cwd ?stdin ?stdout ?stderr ?env ?executable args =
    let executable =
      match executable, args with
      | Some x, _ | None, x :: _ -> x
      | None, [] -> invalid_arg "Arguments list is empty and no executable given!"
    in
    Switch.check sw;
    Switch.run @@ fun setup ->
    let forwards = ref [] in
    let prepare ~input flow forward =
      let fd = Eio_unix.Resource.fd_opt flow in
      match fd with
      | Some fd when Fd.use_exn "spawn" fd stdio_compatible -> flow
      | _ ->
        let r, w = Iocp.pipe ~read_async:(not input) ~write_async:input () in
        let child, parent = if input then r, w else w, r in
        let child = Fd.of_unix ~sw:setup ~blocking:true ~close_unix:true child in
        let cleanup = Switch.on_release_cancellable setup (fun () -> Unix.close parent) in
        let start process =
          let run borrowed =
            Switch.run @@ fun sw ->
            Switch.remove_hook cleanup;
            let wrap ~close_unix fd = Fd.of_unix ~sw ~blocking:false ~close_unix fd in
            let parent = Flow.of_fd ~kind:File_io.Pipe (wrap ~close_unix:true parent) in
            let flow = match borrowed with None -> flow | Some fd -> Flow.of_fd (wrap ~close_unix:false fd) in
            forward process flow parent
          in
          (* Take the borrow before the caller can close its stream. *)
          match fd with
          | None -> run None
          | Some fd -> Fd.use_exn "stdio forwarding" fd (fun raw -> run (Some raw))
        in
        forwards := start :: !forwards;
        Flow.of_fd ~kind:File_io.Pollable child
    in
    let input source =
      prepare ~input:true (source :> Eio.Flow.source_ty r) (fun process source sink ->
          Fiber.first
            (fun () -> copy_stdin source sink)
            (fun () -> ignore (Eio.Process.await process)))
    in
    let output sink =
      prepare ~input:false (sink :> Eio.Flow.sink_ty r) (fun _ sink source -> Eio.Flow.copy source sink)
    in
    let stdin = Option.map input stdin in
    let stdout = Option.map output stdout and stderr = Option.map output stderr in
    let process = spawn v ~sw ?cwd ?stdin ?stdout ?stderr ?env ~executable args in
    List.iter (fun start ->
        Fiber.fork ~sw (fun () -> start process);
        Switch.check sw
      ) (List.rev !forwards);
    process
end

let mgr : Eio_unix.Process.mgr_ty r =
  let h = Eio_unix.Process.Pi.mgr_unix (module Impl) in
  Eio.Resource.T ((), h)
