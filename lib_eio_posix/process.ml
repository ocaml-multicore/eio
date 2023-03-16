open Eio.Std

type t = {
  pid : int;
  exit_status : Unix.process_status Promise.t;
}

module Fork_action = struct
  type t = Eio_unix.Private.Fork_action.t

  let fchdir fd : t =
    { run = fun k ->
          Fd.use_exn "fchdir" fd @@ fun fd ->
          (Eio_unix.Private.Fork_action.fchdir fd).run k
    }

  let chdir = Eio_unix.Private.Fork_action.chdir
  let execve = Eio_unix.Private.Fork_action.execve
end

module Children = struct
  (* Keep track of running child processes and notify their fiber when they exit.
     After forking a child process, it gets registered in the global [db] along with a resolver
     for the promise of its exit status. When we get a SIGCHLD signal, we reap all exited processes
     and resolve their promises, waking whichever fibers are waiting for them.

     We have to be careful not to use a PID after [wait] reaps it, as the PID could have been reused by then.

     The signal handler can run in any domain or systhread, so we have to be careful about that too.
     We can't defer the call to [wait] until we're running in an Eio domain as we don't know which domain
     should handle it until [wait] gives as the process ID. We don't want to delegate to a particular domain
     because it might be spinning doing CPU stuff for a long time. Instead, we try to take the lock in the
     signal handler and do it there. If we can't get the lock then we just record that a wait is needed;
     whoever holds the lock will soon release it and will do the reaping for us.

     Note that, since signal handlers are global,
     this will interfere with any libraries trying to manage processes themselves.

     For systems with Process Descriptors we could skip all this nonsense and
     just poll on the process's FD. e.g. using [pdfork] on FreeBSD or [CLONE_PIDFD] on Linux. *)

  (* Each child process is registered in this table.
     Must hold [pid_lock] when accessing it. *)
  let db : (int, Unix.process_status Promise.u) Hashtbl.t = Hashtbl.create 10

  (* Set to [true] when we receive [SIGCHLD] and [false] before calling [wait]. *)
  let need_wait = Atomic.make false

  (* [pid_lock] must be held when spawning or reaping. Otherwise, this can happen:

     - We spawn process 100, adding it to [db].
     - It exits, sending us SIGCHLD.
     - The signal handler calls [wait], reaping it.
     - Another domain spawns another process 100 and adds it to [db],
       overwriting the previous entry.
     - The signal handler resumes, and gets the wrong entry.

     If [pid_lock] is already locked when the SIGCHLD handler runs then it just leaves [need_wait = true]
     (a signal handler can't wait on a mutex, since it may have interrupted the holder).
     The unlocker needs to check [need_wait] after releasing the lock. *)
  let lock = Mutex.create ()

  (* [pid] has exited. Notify the waiter. Must hold [lock] when calling this. *)
  let report_child_status pid status =
    match Hashtbl.find_opt db pid with
    | Some r ->
      Hashtbl.remove db pid;
      Promise.resolve r status
    | None ->
      (* Not one of ours. Not much we can do here. The spawner will probably get
         an [ECHILD] error when they wait, which will do for the error. *)
      ()

  (* Must hold [lock] when calling this. *)
  let rec reap () =
    Atomic.set need_wait false;
    match Unix.(waitpid [WNOHANG] (-1)) with
    | 0, _ -> ()    (* Returned if there are children but none has exited yet. *)
    | pid, status -> report_child_status pid status; reap ()
    | exception Unix.Unix_error (EINTR, _, _) -> reap ()
    | exception Unix.Unix_error (ECHILD, _, _) -> ()  (* Returned if there are no children at all. *)

  let rec reap_nonblocking () =
    if Mutex.try_lock lock then (
      reap ();
      Mutex.unlock lock;
      if Atomic.get need_wait then reap_nonblocking ()
    ) (* else the unlocker will see [need_wait] and call us later *)

  let unlock () =
    Mutex.unlock lock;
    if Atomic.get need_wait then reap_nonblocking ()

  (* Must hold [lock] when calling this. *)
  let register pid =
    assert (not (Hashtbl.mem db pid));
    let p, r = Promise.create () in
    Hashtbl.add db pid r;
    p

  let with_lock fn =
    Mutex.lock lock;
    Fun.protect fn ~finally:unlock
end

let handle_sigchld () =
  Atomic.set Children.need_wait true;
  Children.reap_nonblocking ()

(* Read a (typically short) error message from a child process. *)
let rec read_response fd =
  let buf = Bytes.create 256 in
  match Low_level.read fd buf 0 (Bytes.length buf) with
  | 0 -> ""
  | n -> Bytes.sub_string buf 0 n ^ read_response fd

let signal t signal =
  (* The lock here ensures we don't signal the PID after reaping it. *)
  Children.with_lock @@ fun () ->
  if not (Promise.is_resolved t.exit_status) then (
    Unix.kill t.pid signal
  )

let with_pipe fn =
  Switch.run @@ fun sw ->
  let r, w = Low_level.pipe ~sw in
  fn r w

external eio_spawn : Unix.file_descr -> Eio_unix.Private.Fork_action.c_action list -> int = "caml_eio_posix_spawn"

let spawn ~sw actions =
  with_pipe @@ fun errors_r errors_w ->
  Eio_unix.Private.Fork_action.with_actions actions @@ fun c_actions ->
  Switch.check sw;
  let t =
    (* We take the lock to ensure that the signal handler won't reap the
       process before we've registered it. *)
    Children.with_lock (fun () ->
        let pid =
          Fd.use_exn "errors-w" errors_w @@ fun errors_w ->
          eio_spawn errors_w c_actions
        in
        Fd.close errors_w;
        { pid; exit_status = Children.register pid }
      )
  in
  let hook = Switch.on_release_cancellable sw (fun () -> signal t Sys.sigkill) in
  (* Removing the hook must be done from our own domain, not from the signal handler,
     so fork a fiber to deal with that. If the switch gets cancelled then this won't
     run, but then the [on_release] handler will run the hook soon anyway. *)
  Fiber.fork_daemon ~sw (fun () ->
      ignore (Promise.await t.exit_status : Unix.process_status);
      Switch.remove_hook hook;
      `Stop_daemon
    );
  (* Check for errors starting the process. *)
  match read_response errors_r with
  | "" -> t                       (* Success! Execing the child closed [errors_w] and we got EOF. *)
  | err -> failwith err
