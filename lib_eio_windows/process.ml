open Eio.Std

module Fd = Eio_unix.Fd

external eio_spawn :
  string option -> string array ->
  Unix.file_descr -> Unix.file_descr -> Unix.file_descr ->
  string -> int * Unix.file_descr
  = "caml_eio_windows_spawn_bytes" "caml_eio_windows_spawn"

external eio_process_wait : Unix.file_descr -> int = "caml_eio_windows_process_wait"
external eio_process_terminate : Unix.file_descr -> int -> bool = "caml_eio_windows_process_terminate"

let check_env env =
  Array.iter
    (fun s ->
       if s = "" || String.contains s '\000' then
         Fmt.invalid_arg "spawn: invalid environment entry %S" s)
    env;
  env

(* Follow OCaml stdlib and only quote only where needed, as apparently cmd.exe
   treats quoted arguments differently even if they dont have spaces. *)
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
    resolve : (int, exn) result Promise.u;
    mutable waiting : bool;
    mutable hook : Switch.hook;         (* Removed once the process has been reaped. *)
  }
  type tag = [ `Generic | `Unix ]

  let pid t = t.pid

  let start_waiting t =
    t.waiting <- true;
    Fd.use_exn "process_wait" t.handle @@ fun h ->
    ignore (
      Sched.enter (fun sched k ->
          Sched.await_thread sched k ~finished:(Promise.resolve t.resolve)
            (fun () -> eio_process_wait h))
      : int)

  let await t =
    if not t.waiting then start_waiting t;
    let code =
      match Promise.await t.exited with
      | Ok code -> code
      | Error ex -> raise ex
    in
    ignore (Switch.try_remove_hook t.hook : bool);
    t.hook <- Switch.null_hook;
    match t.signalled with
    | Some signum when code = terminated_exit_code -> `Signaled signum
    | _ -> `Exited code

  (* Windows has no signals: any signal terminates the process. *)
  let signal t signum =
    Fd.use t.handle ~if_closed:ignore (fun h ->
        if eio_process_terminate h terminated_exit_code then t.signalled <- Some signum)

  let stop t =
    Eio.Cancel.protect @@ fun () ->
    signal t Sys.sigkill;
    ignore (await t : Eio.Process.exit_status)
end

let process_handler = Eio.Process.Pi.process (module Process)
let process t = Eio.Resource.T (t, process_handler)

module Impl = struct
  module T = struct
    type t = unit

    (* CreateProcess takes a Win32 path and not NT object, so we must restrict this.
       It may be possible to use NtCreateUserProcess directly in the future but
       the details are pretty complicated so I didnt go into it just yet. *)
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
      List.iter (fun (i, _, _) ->
          if i > 2 then Fmt.invalid_arg "spawn: only fds 0-2 are supported on Windows (got fd %d)" i)
        fds;
      let cmdline =
        command_line (executable :: (match args with [] -> [] | _ :: tl -> tl))
      in
      let env = check_env env in
      (* Anything not listed is inherited, as on POSIX. *)
      let get n default =
        Option.value ~default (List.find_map (fun (i, fd, _) -> if i = n then Some fd else None) fds)
      in
      let stdin_fd = get 0 Fd.stdin and stdout_fd = get 1 Fd.stdout and stderr_fd = get 2 Fd.stderr in
      let cwd = Option.map cwd_path cwd in
      let pid, raw_handle =
        Fd.use_exn "stdin" stdin_fd @@ fun h0 ->
        Fd.use_exn "stdout" stdout_fd @@ fun h1 ->
        Fd.use_exn "stderr" stderr_fd @@ fun h2 ->
        eio_spawn cwd env h0 h1 h2 cmdline
      in
      let handle = Fd.of_unix ~sw ~blocking:true ~close_unix:true raw_handle in
      let exited, resolve = Promise.create () in
      let t = { Process.pid; handle; signalled = None; exited; resolve;
                waiting = false; hook = Switch.null_hook } in
      t.hook <- Switch.on_release_cancellable sw (fun () -> Process.stop t);
      process t
  end

  include Eio_unix.Process.Make_mgr (T)

  (* CreateProcess resolves the program itself, so the head of the cmdline is used. *)
  let spawn v ~sw ?cwd ?stdin ?stdout ?stderr ?env ?executable args =
    let executable =
      match executable, args with
      | Some x, _ | None, x :: _ -> x
      | None, [] -> invalid_arg "Arguments list is empty and no executable given!"
    in
    spawn v ~sw ?cwd ?stdin ?stdout ?stderr ?env ~executable args
end

let mgr : Eio_unix.Process.mgr_ty r =
  let h = Eio_unix.Process.Pi.mgr_unix (module Impl) in
  Eio.Resource.T ((), h)
