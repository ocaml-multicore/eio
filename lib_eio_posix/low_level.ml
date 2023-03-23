open Eio.Std

(* There are some things that should be improved here:

   - Blocking FDs (e.g. stdout) wait for the FD to become ready and then do a blocking operation.
     This might not succeed, and will block the whole domain in that case.
     Ideally, all blocking operations should happen in a sys-thread instead.

   - Various other operations, such as listing a directory, should also be done in a sys-thread
     to avoid high latencies in the main domain. *)

type ty = Read | Write

module Fd = Fd

let await_readable fd =
  Fd.use_exn "await_readable" fd @@ fun fd ->
  Sched.enter @@ fun t k ->
  Sched.await_readable t k fd

let await_writable fd =
  Fd.use_exn "await_writable" fd @@ fun fd ->
  Sched.enter @@ fun t k ->
  Sched.await_writable t k fd

let rec do_nonblocking ty fn fd =
  try fn fd with
  | Unix.Unix_error (EINTR, _, _) -> do_nonblocking ty fn fd    (* Just in case *)
  | Unix.Unix_error((EAGAIN | EWOULDBLOCK), _, _) ->
    Sched.enter (fun t k ->
        match ty with
        | Read -> Sched.await_readable t k fd
        | Write -> Sched.await_writable t k fd
      );
    do_nonblocking ty fn fd

let read fd buf start len =
  if Fd.is_blocking fd then await_readable fd;
  Fd.use_exn "read" fd @@ fun fd ->
  do_nonblocking Read (fun fd -> Unix.read fd buf start len) fd

let write fd buf start len =
  if Fd.is_blocking fd then await_writable fd;
  Fd.use_exn "write" fd @@ fun fd ->
  do_nonblocking Write (fun fd -> Unix.write fd buf start len) fd

let sleep_until time =
  Sched.enter @@ fun t k ->
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
    await_writable fd;
    match Fd.use_exn "connect" fd Unix.getsockopt_error with
    | None -> ()
    | Some code -> raise (Err.wrap code "connect-in-progress" "")

let accept ~sw sock =
  Fd.use_exn "accept" sock @@ fun sock ->
  let client, addr =
    do_nonblocking Read (fun fd -> Switch.check sw; Unix.accept ~cloexec:true fd) sock
  in
  Unix.set_nonblock client;
  Fd.of_unix ~sw ~blocking:false ~close_unix:true client, addr

let shutdown sock cmd =
  Fd.use_exn "shutdown" sock (fun fd -> Unix.shutdown fd cmd)

let send_msg fd ~dst buf =
  Fd.use_exn "send_msg" fd @@ fun fd ->
  do_nonblocking Write (fun fd -> Unix.sendto fd buf 0 (Bytes.length buf) [] dst) fd

let recv_msg fd buf =
  Fd.use_exn "recv_msg" fd @@ fun fd ->
  do_nonblocking Read (fun fd -> Unix.recvfrom fd buf 0 (Bytes.length buf) []) fd

external eio_getrandom : Cstruct.buffer -> int -> int -> int = "caml_eio_posix_getrandom"

let getrandom { Cstruct.buffer; off; len } =
  let rec loop n =
    if n = len then
      ()
    else
      loop (n + eio_getrandom buffer (off + n) (len - n))
  in
  loop 0

let fstat fd =
  Fd.use_exn "fstat" fd Unix.LargeFile.fstat

let lstat = Unix.LargeFile.lstat

let realpath = Unix.realpath

let read_entries h =
  let rec aux acc =
    match Unix.readdir h with
    | "." | ".." -> aux acc
    | leaf -> aux (leaf :: acc)
    | exception End_of_file -> Array.of_list acc
  in
  aux []

let readdir path =
  let h = Unix.opendir path in
  match read_entries h with
  | r -> Unix.closedir h; r
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    Unix.closedir h; Printexc.raise_with_backtrace ex bt

external eio_readv : Unix.file_descr -> Cstruct.t array -> int = "caml_eio_posix_readv"
external eio_writev : Unix.file_descr -> Cstruct.t array -> int = "caml_eio_posix_writev"

external eio_preadv : Unix.file_descr -> Cstruct.t array -> Optint.Int63.t -> int = "caml_eio_posix_preadv"
external eio_pwritev : Unix.file_descr -> Cstruct.t array -> Optint.Int63.t -> int = "caml_eio_posix_pwritev"

let readv fd bufs =
  if Fd.is_blocking fd then await_readable fd;
  Fd.use_exn "readv" fd @@ fun fd ->
  do_nonblocking Read (fun fd -> eio_readv fd bufs) fd

let writev fd bufs =
  if Fd.is_blocking fd then await_writable fd;
  Fd.use_exn "writev" fd @@ fun fd ->
  do_nonblocking Write (fun fd -> eio_writev fd bufs) fd

let preadv ~file_offset fd bufs =
  if Fd.is_blocking fd then await_readable fd;
  Fd.use_exn "preadv" fd @@ fun fd ->
  do_nonblocking Read (fun fd -> eio_preadv fd bufs file_offset) fd

let pwritev ~file_offset fd bufs =
  if Fd.is_blocking fd then await_writable fd;
  Fd.use_exn "pwritev" fd @@ fun fd ->
  do_nonblocking Write (fun fd -> eio_pwritev fd bufs file_offset) fd

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

  let empty = 0
  let ( + ) = ( lor )
end

let rec with_dirfd op dirfd fn =
  match dirfd with
  | None -> fn (Obj.magic Config.at_fdcwd : Unix.file_descr)
  | Some dirfd -> Fd.use_exn op dirfd fn
  | exception Unix.Unix_error(Unix.EINTR, _, "") -> with_dirfd op dirfd fn

external eio_openat : Unix.file_descr -> string -> Open_flags.t -> int -> Unix.file_descr = "caml_eio_posix_openat"

let openat ?dirfd ~sw ~mode path flags =
  with_dirfd "openat" dirfd @@ fun dirfd ->
  Switch.check sw;
  eio_openat dirfd path Open_flags.(flags + cloexec + nonblock) mode
  |> Fd.of_unix ~sw ~blocking:false ~close_unix:true

external eio_mkdirat : Unix.file_descr -> string -> Unix.file_perm -> unit = "caml_eio_posix_mkdirat"

let mkdir ?dirfd ~mode path =
  with_dirfd "mkdirat" dirfd @@ fun dirfd ->
  eio_mkdirat dirfd path mode

external eio_unlinkat : Unix.file_descr -> string -> bool -> unit = "caml_eio_posix_unlinkat"

let unlink ?dirfd ~dir path =
  with_dirfd "unlink" dirfd @@ fun dirfd ->
  eio_unlinkat dirfd path dir

external eio_renameat : Unix.file_descr -> string -> Unix.file_descr -> string -> unit = "caml_eio_posix_renameat"

let rename ?old_dir old_path ?new_dir new_path =
  with_dirfd "rename-old" old_dir @@ fun old_dir ->
  with_dirfd "rename-new" new_dir @@ fun new_dir ->
  eio_renameat old_dir old_path new_dir new_path

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
  }

  let exit_status t = t.exit_status
  let pid t = t.pid

  module Fork_action = struct
    type t = Eio_unix.Private.Fork_action.t

    let fchdir fd = Eio_unix.Private.Fork_action.fchdir (Fd.to_rcfd fd)
    let chdir = Eio_unix.Private.Fork_action.chdir
    let execve = Eio_unix.Private.Fork_action.execve

    let inherit_fds m : t =
      m
      |> List.map (fun (dst, src, flags) -> (dst, Fd.to_rcfd src, flags))
      |> Eio_unix.Private.Fork_action.inherit_fds
  end

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
    (* The lock here ensures we don't signal the PID after reaping it. *)
    Children.with_lock @@ fun () ->
    if not (Promise.is_resolved t.exit_status) then (
      Unix.kill t.pid signal
    )

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
end
