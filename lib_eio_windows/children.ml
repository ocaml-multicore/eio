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

open Eio.Std

(* Each child process is registered in this table.
   Must hold [lock] when accessing it. *)
let db : (int, Unix.process_status Promise.u) Hashtbl.t = Hashtbl.create 10

(* Set to [true] when we receive [SIGCHLD] and [false] before calling [wait]. *)
let need_wait = Atomic.make false

(* [lock] must be held when spawning or reaping. Otherwise, this can happen:

   - We spawn process 100, adding it to [db].
   - It exits, sending us SIGCHLD.
   - The signal handler calls [wait], reaping it.
   - Another domain spawns another process 100 and adds it to [db],
     overwriting the previous entry.
   - The signal handler resumes, and gets the wrong entry.

   If [lock] is already locked when the SIGCHLD handler runs then it just leaves [need_wait = true]
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

let handle_sigchld () =
  Atomic.set need_wait true;
  reap_nonblocking ()
