open Eio.Std

(* There are some things that should be improved here:

   - Blocking FDs (e.g. stdout) wait for the FD to become ready and then do a blocking operation.
     This might not succeed, and will block the whole domain in that case.
     Ideally, all blocking operations should happen in a sys-thread instead.

   - Various other operations, such as listing a directory, should also be done in a sys-thread
     to avoid high latencies in the main domain. *)

type ty = Read | Write

module Fd = Eio_unix.Fd
module Trace = Eio.Private.Trace
module Fiber_context = Eio.Private.Fiber_context

type dir_fd =
  | Fd of Fd.t
  | Cwd         (* Confined to "." *)
  | Fs          (* Unconfined "."; also allows absolute paths *)

let in_worker_thread label = Eio_unix.run_in_systhread ~label

let await_readable op fd =
  Fd.use_exn "await_readable" fd @@ fun fd ->
  Sched.enter op @@ fun t k ->
  Sched.await_readable t k fd

let await_writable op fd =
  Fd.use_exn "await_writable" fd @@ fun fd ->
  Sched.enter op @@ fun t k ->
  Sched.await_writable t k fd

let rec do_nonblocking ty op fn fd =
  try fn fd with
  | Unix.Unix_error (EINTR, _, _) -> do_nonblocking ty op fn fd    (* Just in case *)
  | Unix.Unix_error((EAGAIN | EWOULDBLOCK), _, _) ->
    Sched.enter op (fun t k ->
        match ty with
        | Read -> Sched.await_readable t k fd
        | Write -> Sched.await_writable t k fd
      );
    do_nonblocking ty op fn fd

let do_nonblocking ty op fn fd =
  Fiber.yield ();
  Trace.with_span op (fun () -> do_nonblocking ty op fn fd)

let read fd buf start len =
  if Fd.is_blocking fd then await_readable "read" fd;
  Fd.use_exn "read" fd @@ fun fd ->
  do_nonblocking Read "read" (fun fd -> Unix.read fd buf start len) fd

let write fd buf start len =
  if Fd.is_blocking fd then await_writable "write" fd;
  Fd.use_exn "write" fd @@ fun fd ->
  do_nonblocking Write "write" (fun fd -> Unix.write fd buf start len) fd

let sleep_until time =
  Sched.enter "sleep" @@ fun t k ->
  Sched.await_timeout t k time

let socket ~sw socket_domain socket_type protocol =
  Switch.check sw;
  let sock_unix = Unix.socket ~cloexec:true socket_domain socket_type protocol in
  Unix.set_nonblock sock_unix;
  Fd.of_unix ~sw ~blocking:false ~close_unix:true sock_unix

let connect fd addr =
  try
    Fd.use_exn "connect" fd (fun fd -> Unix.connect fd addr)
  with
  | Unix.Unix_error ((EINTR | EAGAIN | EWOULDBLOCK | EINPROGRESS), _, _) ->
    await_writable "connect" fd;
    match Fd.use_exn "connect" fd Unix.getsockopt_error with
    | None -> ()
    | Some code -> raise (Err.wrap code "connect-in-progress" "")

let accept ~sw sock =
  Fd.use_exn "accept" sock @@ fun sock ->
  let client, addr =
    do_nonblocking Read "accept" (fun fd -> Switch.check sw; Unix.accept ~cloexec:true fd) sock
  in
  Unix.set_nonblock client;
  Fd.of_unix ~sw ~blocking:false ~close_unix:true client, addr

let shutdown sock cmd =
  Fd.use_exn "shutdown" sock (fun fd -> Unix.shutdown fd cmd)

external eio_send_msg : Unix.file_descr -> int -> Unix.file_descr list -> Unix.sockaddr option -> Cstruct.t array -> int = "caml_eio_posix_send_msg"
external eio_recv_msg : Unix.file_descr -> int -> Cstruct.t array -> Unix.sockaddr * int * Unix.file_descr list = "caml_eio_posix_recv_msg"

let send_msg fd ?(fds = []) ?dst buf =
  Fd.use_exn "send_msg" fd @@ fun fd ->
  Fd.use_exn_list "send_msg" fds @@ fun fds ->
  do_nonblocking Write "send_msg" (fun fd -> eio_send_msg fd (List.length fds) fds dst buf) fd

let recv_msg fd buf =
  let addr, got, _ =
    Fd.use_exn "recv_msg" fd @@ fun fd ->
    do_nonblocking Read "recv_msg" (fun fd -> eio_recv_msg fd 0 buf) fd
  in
  (addr, got)

let recv_msg_with_fds ~sw ~max_fds fd buf =
  let addr, got, fds =
    Fd.use_exn "recv_msg" fd @@ fun fd ->
    do_nonblocking Read "recv_msg" (fun fd -> eio_recv_msg fd max_fds buf) fd
  in
  (addr, got, Eio_unix.Fd.of_unix_list ~sw fds)

external eio_getrandom : Cstruct.buffer -> int -> int -> int = "caml_eio_posix_getrandom"

let getrandom { Cstruct.buffer; off; len } =
  let rec loop n =
    if n = len then
      ()
    else
      loop (n + eio_getrandom buffer (off + n) (len - n))
  in
  in_worker_thread "getrandom" @@ fun () ->
  loop 0

let realpath path =
  in_worker_thread "realpath" @@ fun () ->
  Unix.realpath path

let read_entries h =
  let rec aux acc =
    match Unix.readdir h with
    | "." | ".." -> aux acc
    | leaf -> aux (leaf :: acc)
    | exception End_of_file -> Array.of_list acc
  in
  aux []

external eio_readv : Unix.file_descr -> Cstruct.t array -> int = "caml_eio_posix_readv"
external eio_writev : Unix.file_descr -> Cstruct.t array -> int = "caml_eio_posix_writev"

external eio_preadv : Unix.file_descr -> Cstruct.t array -> Optint.Int63.t -> int = "caml_eio_posix_preadv"
external eio_pwritev : Unix.file_descr -> Cstruct.t array -> Optint.Int63.t -> int = "caml_eio_posix_pwritev"

let readv fd bufs =
  if Fd.is_blocking fd then await_readable "readv" fd;
  Fd.use_exn "readv" fd @@ fun fd ->
  do_nonblocking Read "readv" (fun fd -> eio_readv fd bufs) fd

let writev fd bufs =
  if Fd.is_blocking fd then await_writable "writev" fd;
  Fd.use_exn "writev" fd @@ fun fd ->
  do_nonblocking Write "writev" (fun fd -> eio_writev fd bufs) fd

let preadv ~file_offset fd bufs =
  if Fd.is_blocking fd then await_readable "preadv" fd;
  Fd.use_exn "preadv" fd @@ fun fd ->
  do_nonblocking Read "preadv" (fun fd -> eio_preadv fd bufs file_offset) fd

let pwritev ~file_offset fd bufs =
  if Fd.is_blocking fd then await_writable "pwritev" fd;
  Fd.use_exn "pwritev" fd @@ fun fd ->
  do_nonblocking Write "pwritev" (fun fd -> eio_pwritev fd bufs file_offset) fd

module Open_flags = struct
  type t = int

  let rdonly = Config.o_rdonly
  let rdwr = Config.o_rdwr
  let wronly = Config.o_wronly
  let append = Config.o_append
  let cloexec = Config.o_cloexec
  let creat = Config.o_creat
  let directory = Config.o_directory
  let dsync = Config.o_dsync
  let excl = Config.o_excl
  let noctty = Config.o_noctty
  let nofollow = Config.o_nofollow
  let nonblock = Config.o_nonblock
  let sync = Config.o_sync
  let trunc = Config.o_trunc
  let resolve_beneath = Config.o_resolve_beneath
  let path = Config.o_path

  let empty = 0
  let ( + ) = ( lor )

  let ( +? ) x = function
    | None -> x
    | Some y -> x + y
end

let at_fdcwd : Unix.file_descr = Obj.magic Config.at_fdcwd

external eio_openat : Unix.file_descr -> string -> Open_flags.t -> int -> Unix.file_descr = "caml_eio_posix_openat"
let eio_openat fd path flags mode =
  let fd = Option.value fd ~default:at_fdcwd in
  eio_openat fd path Open_flags.(flags + cloexec) mode

module Resolve = struct
  (** Resolve a path one step at a time.
      This simulates how the kernel does path resolution using O_RESOLVE_BENEATH,
      for kernels that don't support it.

      These functions should be called from a worker sys-thread, since lookups can
      be slow, especially on network file-systems and user-space mounts.

      When doing lookups, we cannot ask the kernel to follow ".." links, since the
      directory might get moved during the operation. e.g.

      Process 1: openat [/tmp/sandbox/] "foo/../bar"
      Process 2: mv /tmp/sandbox/foo /var/foo

      Process 1 starts by opening "foo", then process 2 moves it, then process 1
      follows the "../bar", opening /var/bar, to which it should not have access.
      Instead, we keep a stack of opened directories and pop one when we see "..".
      todo: possibly we should check we have search permission on ".." before
      doing this.
  *)

  type dir_stack =
    | Base of Unix.file_descr option          (* Base dir from user (do not close). None if cwd *)
    | Tmp of Unix.file_descr * dir_stack      (* Will be closed if in [dir_stack] at end. *)

  type state = {
    mutable dir_stack : dir_stack;            (* Directories already opened, for ".." *)
    mutable max_follows : int;                (* Max symlinks before reporting ELOOP *)
  }

  let current_dir state =
    match state.dir_stack with
    | Base b -> b
    | Tmp (x, _) -> Some x

  let parse_rel s =
    match Path.parse s with
    | Relative r -> r
    | Absolute _ -> raise @@ Eio.Fs.err (Eio.Fs.Permission_denied (Err.Absolute_path))

  let decr_max_follows state x =
    if state.max_follows > 0 then
      state.max_follows <- state.max_follows - 1
    else
      raise (Unix.Unix_error (ELOOP, "resolve", x))

  (* Fallback for systems without O_RESOLVE_BENEATH: *)
  let rec resolve state (path : Path.Rel.t) =
    (* traceln "Consider %a" Path.Rel.dump path; *)
    match path with
    | Leaf { basename; trailing_slash } -> if trailing_slash then basename ^ "/" else basename
    | Self -> "."
    | Parent xs ->
      begin match state.dir_stack with
        | Base _ ->
          raise @@ Eio.Fs.err (Permission_denied (Err.Outside_sandbox (Path.Rel.to_string path)))
        | Tmp (p, ps) ->
          Unix.close p;
          state.dir_stack <- ps;
          resolve state xs
      end
    | Child (x, xs) ->
      let base = current_dir state in
      match eio_openat base x Open_flags.(nofollow + directory +? path) 0 with
      | new_base ->
        state.dir_stack <- Tmp (new_base, state.dir_stack);
        resolve state xs
      | exception (Unix.Unix_error ((ENOTDIR | EMLINK | EUNKNOWNERR _), _, _) as e) ->
        match Eio_unix.Private.read_link_unix base x with
        | target ->
          decr_max_follows state x;
          resolve state (Path.Rel.concat (parse_rel target) xs)
        | exception Unix.Unix_error _ -> raise e (* Not a symlink; report original error instead *)

  let close_tmp state =
    let rec aux = function
      | Base _ -> ()
      | Tmp (x, xs) -> Unix.close x; aux xs
    in
    aux state.dir_stack

  let with_state base fn =
    (* [max_follows] matches Linux's value; see path_resolution(7) *)
    let state = { dir_stack = Base base; max_follows = 40 } in
    match fn state with
    | x -> close_tmp state; x
    | exception ex ->
      let bt = Printexc.get_raw_backtrace () in
      close_tmp state;
      Printexc.raise_with_backtrace ex bt

  let trailing_slash x =
    x <> "" && x.[String.length x - 1] = '/'

  let open_beneath_fallback ?dirfd:base ~sw ~mode path flags =
    let path = parse_rel path in
    with_state base @@ fun state ->
    (* Resolve the parent, then try to open the last component with [flags + nofollow].
       If it's a symlink, retry with the target. *)
    let rec aux leaf =
      let base = current_dir state in
      let flags = if trailing_slash leaf then Open_flags.(flags + directory) else flags in
      match eio_openat base leaf Open_flags.(flags + nofollow) mode with
      | fd -> Fd.of_unix fd ~sw ~blocking:false ~close_unix:true
      | exception (Unix.Unix_error ((ELOOP | ENOTDIR | EMLINK | EUNKNOWNERR _), _, _) as e) ->
        (* Note: Linux uses ELOOP or ENOTDIR. FreeBSD uses EMLINK. NetBSD uses EFTYPE. *)
        match Eio_unix.Private.read_link_unix base leaf with
        | target ->
          decr_max_follows state leaf;
          aux (resolve state (parse_rel target))
        | exception Unix.Unix_error _ -> raise e
    in
    aux (resolve state path)

  (* Resolve until the last component and call [fn dir leaf].
     That returns [Error `Symlink] if [leaf] is a symlink, in
     which case we read its target and continue. *)
  let with_parent_loop ?dirfd:base path fn =
    let path = parse_rel path in
    with_state base @@ fun state ->
    let rec aux leaf =
      let base = current_dir state in
      match fn base leaf with
      | Ok x -> x
      | Error (`Symlink e) ->
        decr_max_follows state leaf;
        match Eio_unix.Private.read_link_unix base leaf with
        | target -> aux (resolve state (parse_rel target))
        | exception Unix.Unix_error _ when Option.is_some e -> raise (Option.get e)
    in
    aux (resolve state path)

  (* If confined, resolve until the last component and call [fn dir leaf].
     If unconfined, just call [fn None path].
     If you need to follow [leaf] if it turns out to be a symlink,
     use [with_parent_loop] instead. *)
  let with_parent op fd path fn = (* todo: use o_resolve_beneath if available *)
    match fd with
    | Fs -> fn None path
    | Cwd -> with_parent_loop path (fun x y -> Ok (fn x y))
    | Fd dirfd ->
      Fd.use_exn op dirfd @@ fun dirfd ->
      with_parent_loop ~dirfd path (fun x y -> Ok (fn x y))

  let open_unconfined ~sw ~mode dirfd path flags =
    let flags = if trailing_slash path then Open_flags.(flags + directory) else flags in
    Fd.use_exn_opt "openat" dirfd @@ fun dirfd ->
    eio_openat dirfd path Open_flags.(flags + nonblock) mode
    |> Fd.of_unix ~sw ~blocking:false ~close_unix:true

  let open_beneath ?dirfd ~sw ~mode path flags =
    match Open_flags.resolve_beneath with
    | Some o_resolve_beneath ->
      open_unconfined ~sw ~mode dirfd path Open_flags.(flags + o_resolve_beneath)
    | None ->
      Fd.use_exn_opt "open_beneath" dirfd @@ fun dirfd ->
      open_beneath_fallback ?dirfd ~sw ~mode path flags
end

let openat ~sw ~mode fd path flags =
  let path = if path = "" then "." else path in
  in_worker_thread "openat" @@ fun () ->
  match fd with
  | Fs -> Resolve.open_unconfined ~sw ~mode None path flags
  | Cwd -> Resolve.open_beneath ~sw ~mode ?dirfd:None path flags
  | Fd dirfd -> Resolve.open_beneath ~sw ~mode ~dirfd path flags

external eio_fdopendir : Unix.file_descr -> Unix.dir_handle = "caml_eio_posix_fdopendir"

let readdir dirfd path =
  in_worker_thread "readdir" @@ fun () ->
  let use h =
    match read_entries h with
    | r -> Unix.closedir h; r
    | exception ex ->
      let bt = Printexc.get_raw_backtrace () in
      Unix.closedir h;
      Printexc.raise_with_backtrace ex bt
  in
  let use_confined dirfd =
    Resolve.with_parent_loop ?dirfd path @@ fun dirfd path ->
    match eio_openat dirfd path Open_flags.(rdonly + directory + nofollow) 0 with
    | fd -> Ok (use (eio_fdopendir fd))
    | exception (Unix.Unix_error ((ELOOP | ENOTDIR | EMLINK | EUNKNOWNERR _), _, _) as e) -> Error (`Symlink (Some e))
  in
  match dirfd with
  | Fs -> use (Unix.opendir path)
  | Cwd -> use_confined None
  | Fd dirfd ->
    Fd.use_exn "readdir" dirfd @@ fun dirfd ->
    use_confined (Some dirfd)

external eio_mkdirat : Unix.file_descr -> string -> Unix.file_perm -> unit = "caml_eio_posix_mkdirat"

let mkdir ~mode dirfd path =
  in_worker_thread "mkdir" @@ fun () ->
  Resolve.with_parent "mkdir" dirfd path @@ fun dirfd path ->
  let dirfd = Option.value dirfd ~default:at_fdcwd in
  eio_mkdirat dirfd path mode

external eio_unlinkat : Unix.file_descr -> string -> bool -> unit = "caml_eio_posix_unlinkat"

let unlink ~dir dirfd path =
  in_worker_thread "unlink" @@ fun () ->
  Resolve.with_parent "unlink" dirfd path @@ fun dirfd path ->
  let dirfd = Option.value dirfd ~default:at_fdcwd in
  eio_unlinkat dirfd path dir

external eio_renameat : Unix.file_descr -> string -> Unix.file_descr -> string -> unit = "caml_eio_posix_renameat"

let rename old_dir old_path new_dir new_path =
  in_worker_thread "rename" @@ fun () ->
  Resolve.with_parent "rename-old" old_dir old_path @@ fun old_dir old_path ->
  Resolve.with_parent "rename-new" new_dir new_path @@ fun new_dir new_path ->
  let old_dir = Option.value old_dir ~default:at_fdcwd in
  let new_dir = Option.value new_dir ~default:at_fdcwd in
  eio_renameat old_dir old_path new_dir new_path

external eio_symlinkat : string -> Unix.file_descr -> string -> unit = "caml_eio_posix_symlinkat"

let symlink old_path new_dir new_path =
  in_worker_thread "symlink" @@ fun () ->
  Resolve.with_parent "symlink-new" new_dir new_path @@ fun new_dir new_path ->
  let new_dir = Option.value new_dir ~default:at_fdcwd in
  eio_symlinkat old_path new_dir new_path

let read_link dirfd path =
  in_worker_thread "read_link" @@ fun () ->
  Resolve.with_parent "read_link" dirfd path @@ fun dirfd path ->
  Eio_unix.Private.read_link_unix dirfd path

type stat
external create_stat : unit -> stat = "caml_eio_posix_make_stat"
external eio_fstatat : stat -> Unix.file_descr -> string -> int -> unit = "caml_eio_posix_fstatat"
external eio_fstat   : stat -> Unix.file_descr -> unit = "caml_eio_posix_fstat"

external blksize : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_blksize_bytes" "ocaml_eio_posix_stat_blksize_native" [@@noalloc]
external nlink   : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_nlink_bytes" "ocaml_eio_posix_stat_nlink_native" [@@noalloc]
external uid     : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_uid_bytes" "ocaml_eio_posix_stat_uid_native" [@@noalloc]
external gid     : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_gid_bytes" "ocaml_eio_posix_stat_gid_native" [@@noalloc]
external ino     : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_ino_bytes" "ocaml_eio_posix_stat_ino_native" [@@noalloc]
external size    : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_size_bytes" "ocaml_eio_posix_stat_size_native" [@@noalloc]
external rdev    : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_rdev_bytes" "ocaml_eio_posix_stat_rdev_native" [@@noalloc]
external dev     : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_dev_bytes" "ocaml_eio_posix_stat_dev_native" [@@noalloc]
external perm    : stat -> (int [@untagged]) = "ocaml_eio_posix_stat_perm_bytes" "ocaml_eio_posix_stat_perm_native" [@@noalloc]
external mode    : stat -> (int [@untagged]) = "ocaml_eio_posix_stat_mode_bytes" "ocaml_eio_posix_stat_mode_native" [@@noalloc]
external kind    : stat -> Eio.File.Stat.kind = "ocaml_eio_posix_stat_kind"

external atime_sec : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_atime_sec_bytes" "ocaml_eio_posix_stat_atime_sec_native" [@@noalloc]
external ctime_sec : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_ctime_sec_bytes" "ocaml_eio_posix_stat_ctime_sec_native" [@@noalloc]
external mtime_sec : stat -> (int64 [@unboxed]) = "ocaml_eio_posix_stat_mtime_sec_bytes" "ocaml_eio_posix_stat_mtime_sec_native" [@@noalloc]

external atime_nsec : stat -> int = "ocaml_eio_posix_stat_atime_nsec" [@@noalloc]
external ctime_nsec : stat -> int = "ocaml_eio_posix_stat_ctime_nsec" [@@noalloc]
external mtime_nsec : stat -> int = "ocaml_eio_posix_stat_mtime_nsec" [@@noalloc]

let fstat ~buf fd =
  Fd.use_exn "fstat" fd @@ fun fd ->
  eio_fstat buf fd

let fstatat_confined ~buf ~follow dirfd path =
  Resolve.with_parent_loop ?dirfd path @@ fun dirfd path ->
  let dirfd = Option.value dirfd ~default:at_fdcwd in
  eio_fstatat buf dirfd path Config.at_symlink_nofollow;
  if follow && kind buf = `Symbolic_link then Error (`Symlink None) else Ok ()

let fstatat ~buf ~follow dirfd path =
  in_worker_thread "fstat" @@ fun () ->
  match dirfd with
  | Fs ->
    let flags = if follow then 0 else Config.at_symlink_nofollow in
    eio_fstatat buf at_fdcwd path flags
  | Cwd -> fstatat_confined ~buf ~follow None path
  | Fd dirfd ->
    Fd.use_exn "fstat" dirfd @@ fun dirfd ->
    fstatat_confined ~buf ~follow (Some dirfd) path

let lseek fd off cmd =
  Fd.use_exn "lseek" fd @@ fun fd ->
  let cmd =
    match cmd with
    | `Set -> Unix.SEEK_SET
    | `Cur -> Unix.SEEK_CUR
    | `End -> Unix.SEEK_END
  in
  Unix.LargeFile.lseek fd (Optint.Int63.to_int64 off) cmd
  |> Optint.Int63.of_int64

let fsync fd =
  Eio_unix.run_in_systhread ~label:"fsync" @@ fun () ->
  Fd.use_exn "fsync" fd Unix.fsync

let ftruncate fd len =
  Eio_unix.run_in_systhread ~label:"ftruncate" @@ fun () ->
  Fd.use_exn "ftruncate" fd @@ fun fd ->
  Unix.LargeFile.ftruncate fd (Optint.Int63.to_int64 len)

let pipe ~sw =
  let unix_r, unix_w = Unix.pipe ~cloexec:true () in
  let r = Fd.of_unix ~sw ~blocking:false ~close_unix:true unix_r in
  let w = Fd.of_unix ~sw ~blocking:false ~close_unix:true unix_w in
  Unix.set_nonblock unix_r;
  Unix.set_nonblock unix_w;
  r, w

module Process = struct
  type t = {
    pid : int;
    exit_status : Unix.process_status Promise.t;
    lock : Mutex.t;
  }
  (* When [lock] is unlocked, [exit_status] is resolved iff the process has been reaped. *)

  let exit_status t = t.exit_status
  let pid t = t.pid

  module Fork_action = Eio_unix.Private.Fork_action

  (* Read a (typically short) error message from a child process. *)
  let rec read_response fd =
    let buf = Bytes.create 256 in
    match read fd buf 0 (Bytes.length buf) with
    | 0 -> ""
    | n -> Bytes.sub_string buf 0 n ^ read_response fd

  let with_pipe fn =
    Switch.run @@ fun sw ->
    let r, w = pipe ~sw in
    fn r w

  let signal t signal =
    (* We need the lock here so that one domain can't signal the process exactly as another is reaping it. *)
    Mutex.lock t.lock;
    Fun.protect ~finally:(fun () -> Mutex.unlock t.lock) @@ fun () ->
    if not (Promise.is_resolved t.exit_status) then (
      Unix.kill t.pid signal
    ) (* else process has been reaped and t.pid is invalid *)

  external eio_spawn : Unix.file_descr -> Eio_unix.Private.Fork_action.c_action list -> int = "caml_eio_posix_spawn"

  (* Wait for [pid] to exit and then resolve [exit_status] to its status. *)
  let reap t exit_status =
    Eio.Condition.loop_no_mutex Eio_unix.Process.sigchld (fun () ->
        Mutex.lock t.lock;
        match Unix.waitpid [WNOHANG] t.pid with
        | 0, _ -> Mutex.unlock t.lock; None                (* Not ready; wait for next SIGCHLD *)
        | p, status ->
          assert (p = t.pid);
          Promise.resolve exit_status status;
          Mutex.unlock t.lock;
          Some ()
      )

  let spawn ~sw actions =
    with_pipe @@ fun errors_r errors_w ->
    Eio_unix.Private.Fork_action.with_actions actions @@ fun c_actions ->
    Switch.check sw;
    let exit_status, set_exit_status = Promise.create () in
    let t =
      let pid =
        Fd.use_exn "errors-w" errors_w @@ fun errors_w ->
        eio_spawn errors_w c_actions
      in
      Fd.close errors_w;
      { pid; exit_status; lock = Mutex.create () }
    in
    let hook = Switch.on_release_cancellable sw (fun () ->
        (* Kill process (if still running) *)
        signal t Sys.sigkill;
        (* The switch is being released, so either the daemon fiber got
           cancelled or it hasn't started yet (and never will start). *)
        if not (Promise.is_resolved t.exit_status) then (
          (* Do a (non-cancellable) waitpid here to reap the child. *)
          reap t set_exit_status
        )
      ) in
    Fiber.fork_daemon ~sw (fun () ->
        reap t set_exit_status;
        Switch.remove_hook hook;
        `Stop_daemon
      );
    (* Check for errors starting the process. *)
    match read_response errors_r with
    | "" -> t                       (* Success! Execing the child closed [errors_w] and we got EOF. *)
    | err -> failwith err
end
