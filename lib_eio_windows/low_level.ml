open Eio.Std

(* There are some things that should be improved here:

   - Blocking FDs (e.g. stdout) wait for the FD to become ready and then do a blocking operation.
     This might not succeed, and will block the whole domain in that case.
     Ideally, all blocking operations should happen in a sys-thread instead.

   - Various other operations, such as listing a directory, should also be done in a sys-thread
     to avoid high latencies in the main domain. *)

type ty = Read | Write

module Fd = Fd

(* todo: keeping a pool of workers is probably faster *)
let in_worker_thread = Eio_unix.run_in_systhread

let await_readable fd =
  Fd.use_exn "await_readable" fd @@ fun fd ->
  Sched.enter @@ fun t k ->
  Sched.await_readable t k fd

let await_writable fd =
  Fd.use_exn "await_writable" fd @@ fun fd ->
  Sched.enter @@ fun t k ->
  Sched.await_writable t k fd

let rec do_nonblocking ty fn fd =
  Fiber.yield ();
  try fn fd with
  | Unix.Unix_error (EINTR, _, _) -> 
    do_nonblocking ty fn fd    (* Just in case *)
  | Unix.Unix_error((EAGAIN | EWOULDBLOCK), _, _) ->
    Sched.enter (fun t k ->
        match ty with
        | Read -> Sched.await_readable t k fd
        | Write -> Sched.await_writable t k fd
      );
    do_nonblocking ty fn fd

let read fd buf start len =
  Fd.use_exn "read" fd @@ fun fd ->
  do_nonblocking Read (fun fd -> Unix.read fd buf start len) fd

let read_cstruct fd buf =
  Fd.use_exn "read_cstruct" fd @@ fun fd ->
  do_nonblocking Read (fun fd -> Unix_cstruct.read fd buf) fd

let write fd buf start len =
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

external eio_getrandom : Cstruct.buffer -> int -> int -> int = "caml_eio_windows_getrandom"

let getrandom { Cstruct.buffer; off; len } =
  let rec loop n =
    if n = len then
      ()
    else
      loop (n + eio_getrandom buffer (off + n) (len - n))
  in
  in_worker_thread @@ fun () ->
  loop 0

let fstat fd =
  Fd.use_exn "fstat" fd Unix.LargeFile.fstat

let lstat path =
  in_worker_thread @@ fun () ->
  Unix.LargeFile.lstat path

let realpath path =
  in_worker_thread @@ fun () ->
  Unix.realpath path

let read_entries h =
  let rec aux acc =
    match Unix.readdir h with
    | "." | ".." -> aux acc
    | leaf -> aux (leaf :: acc)
    | exception End_of_file -> Array.of_list acc
  in
  aux []

let readdir path =
  in_worker_thread @@ fun () ->
  let h = Unix.opendir path in
  match read_entries h with
  | r -> Unix.closedir h; r
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    Unix.closedir h; Printexc.raise_with_backtrace ex bt

external eio_readv : Unix.file_descr -> Cstruct.t array -> int = "caml_eio_windows_readv"

external eio_preadv : Unix.file_descr -> Cstruct.t array -> Optint.Int63.t -> int = "caml_eio_windows_preadv"
external eio_pwritev : Unix.file_descr -> Cstruct.t array -> Optint.Int63.t -> int = "caml_eio_windows_pwritev"

let readv fd bufs =
  Fd.use_exn "readv" fd @@ fun fd ->
  do_nonblocking Read (fun fd -> eio_readv fd bufs) fd

let writev fd bufs =
  Fd.use_exn "writev" fd @@ fun fd ->
  do_nonblocking Write (fun fd -> Unix_cstruct.writev fd bufs) fd

let preadv ~file_offset fd bufs =
  Fd.use_exn "preadv" fd @@ fun fd ->
  do_nonblocking Read (fun fd -> eio_preadv fd bufs file_offset) fd

let pwritev ~file_offset fd bufs =
  Fd.use_exn "pwritev" fd @@ fun fd ->
  do_nonblocking Write (fun fd -> eio_pwritev fd bufs file_offset) fd

module Open_flags = struct
  type t = int

  let rdonly = Config.o_rdonly
  let rdwr = Config.o_rdwr
  let wronly = Config.o_wronly
  let append = Config.o_append
  let cloexec = Config.o_noinherit
  let creat = Config.o_creat
  let excl = Config.o_excl
  (* let directory = Config.o_directory
  let dsync = Config.o_dsync
  let noctty = Config.o_noctty
  let nofollow = Config.o_nofollow *)
  (* let nonblock = Config.o_nonblock *)
  (* let sync = Config.o_sync *)
  let trunc = Config.o_trunc

  let empty = 0
  let ( + ) = ( lor )
end

let rec with_dirfd op dirfd fn =
  match dirfd with
  | None -> fn (Obj.magic (failwith "TODO AT_FDCWD") : Unix.file_descr)
  | Some dirfd -> Fd.use_exn op dirfd fn
  | exception Unix.Unix_error(Unix.EINTR, _, "") -> with_dirfd op dirfd fn

external eio_openat : Unix.file_descr -> string -> Open_flags.t -> int -> Unix.file_descr = "caml_eio_windows_openat"

let openat ?dirfd ~sw ~mode path flags =
  with_dirfd "openat" dirfd @@ fun dirfd ->
  Switch.check sw;
  in_worker_thread (fun () -> eio_openat dirfd path Open_flags.(flags + cloexec (* + nonblock *)) mode)
  |> Fd.of_unix ~sw ~blocking:false ~close_unix:true

external eio_mkdirat : Unix.file_descr -> string -> Unix.file_perm -> unit = "caml_eio_windows_mkdirat"

let mkdir ?dirfd ~mode path =
  with_dirfd "mkdirat" dirfd @@ fun dirfd ->
  in_worker_thread @@ fun () ->
  eio_mkdirat dirfd path mode

external eio_unlinkat : Unix.file_descr -> string -> bool -> unit = "caml_eio_windows_unlinkat"

let unlink ?dirfd ~dir path =
  with_dirfd "unlink" dirfd @@ fun dirfd ->
  in_worker_thread @@ fun () ->
  eio_unlinkat dirfd path dir

external eio_renameat : Unix.file_descr -> string -> Unix.file_descr -> string -> unit = "caml_eio_windows_renameat"

let rename ?old_dir old_path ?new_dir new_path =
  with_dirfd "rename-old" old_dir @@ fun old_dir ->
  with_dirfd "rename-new" new_dir @@ fun new_dir ->
  in_worker_thread @@ fun () ->
  eio_renameat old_dir old_path new_dir new_path

let pipe ~sw =
  let unix_r, unix_w = Unix.pipe ~cloexec:true () in
  let r = Fd.of_unix ~sw ~blocking:false ~close_unix:true unix_r in
  let w = Fd.of_unix ~sw ~blocking:false ~close_unix:true unix_w in
  Unix.set_nonblock unix_r;
  Unix.set_nonblock unix_w;
  r, w
