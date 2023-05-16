[@@@alert "-unstable"]

open Eio.Std

module Ctf = Eio.Private.Ctf
module Fd = Eio_unix.Fd

type dir_fd =
  | FD of Fd.t
  | Cwd         (* Confined to "." *)
  | Fs          (* Unconfined "."; also allows absolute paths *)

let uring_file_offset t =
  if Fd.is_seekable t then Optint.Int63.minus_one else Optint.Int63.zero

let file_offset t = function
  | Some x -> `Pos x
  | None when Fd.is_seekable t -> `Seekable_current
  | None -> `Nonseekable_current

let enqueue_read st action (file_offset,fd,buf,len) =
  let req = { Sched.op=`R; file_offset; len; fd; cur_off = 0; buf; action } in
  Ctf.label "read";
  Sched.submit_rw_req st req

let rec enqueue_writev args st action =
  let (file_offset,fd,bufs) = args in
  Ctf.label "writev";
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.writev st.uring ~file_offset fd bufs (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_writev args st action) st.io_q

let enqueue_write st action (file_offset,fd,buf,len) =
  let req = { Sched.op=`W; file_offset; len; fd; cur_off = 0; buf; action } in
  Ctf.label "write";
  Sched.submit_rw_req st req

let rec enqueue_splice ~src ~dst ~len st action =
  Ctf.label "splice";
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.splice st.uring (Job action) ~src ~dst ~len
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_splice ~src ~dst ~len st action) st.io_q

let rec enqueue_openat2 ((access, flags, perm, resolve, fd, path) as args) st action =
  Ctf.label "openat2";
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.openat2 st.uring ~access ~flags ~perm ~resolve ?fd path (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_openat2 args st action) st.io_q

let rec enqueue_unlink ((dir, fd, path) as args) st action =
  Ctf.label "unlinkat";
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.unlink st.uring ~dir ~fd path (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_unlink args st action) st.io_q

let rec enqueue_connect fd addr st action =
  Ctf.label "connect";
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.connect st.uring fd addr (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_connect fd addr st action) st.io_q

let rec enqueue_send_msg fd ~fds ~dst buf st action =
  Ctf.label "send_msg";
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.send_msg st.uring fd ~fds ?dst buf (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_send_msg fd ~fds ~dst buf st action) st.io_q

let rec enqueue_recv_msg fd msghdr st action =
  Ctf.label "recv_msg";
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.recv_msg st.uring fd msghdr (Job action);
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_recv_msg fd msghdr st action) st.io_q

let rec enqueue_accept fd client_addr st action =
  Ctf.label "accept";
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.accept st.uring fd client_addr (Job action)
    ) in
  if retry then (
    (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_accept fd client_addr st action) st.io_q
  )

let rec enqueue_noop t action =
  Ctf.label "noop";
  let job = Sched.enqueue_job t (fun () -> Uring.noop t.uring (Job_no_cancel action)) in
  if job = None then (
    (* wait until an sqe is available *)
    Queue.push (fun t -> enqueue_noop t action) t.io_q
  )

let noop () =
  let result = Sched.enter enqueue_noop in
  if result <> 0 then raise (Err.unclassified (Eio_unix.Unix_error (Uring.error_of_errno result, "noop", "")))

let sleep_until time =
  Sched.enter @@ fun t k ->
  let job = Eio_utils.Zzz.add t.sleep_q time k in
  Eio.Private.Fiber_context.set_cancel_fn k.fiber (fun ex ->
      Eio_utils.Zzz.remove t.sleep_q job;
      Sched.enqueue_failed_thread t k ex
    )

let read ?file_offset:off fd buf amount =
  let off = file_offset fd off in
  Fd.use_exn "read" fd @@ fun fd ->
  let res = Sched.enter (fun t k -> enqueue_read t k (off, fd, buf, amount)) in
  if res < 0 then (
    raise @@ Err.wrap (Uring.error_of_errno res) "read" ""
  ) else res

let read_exactly ?file_offset fd buf len =
  ignore (read ?file_offset fd buf (Exactly len) : int)

let read_upto ?file_offset fd buf len =
  read ?file_offset fd buf (Upto len)

let rec enqueue_readv args st action =
  let (file_offset,fd,bufs) = args in
  Ctf.label "readv";
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.readv st.uring ~file_offset fd bufs (Job action))
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_readv args st action) st.io_q

let readv ?file_offset fd bufs =
  let file_offset =
    match file_offset with
    | Some x -> x
    | None -> uring_file_offset fd
  in
  Fd.use_exn "readv" fd @@ fun fd ->
  let res = Sched.enter (enqueue_readv (file_offset, fd, bufs)) in
  if res < 0 then (
    raise @@ Err.wrap (Uring.error_of_errno res) "readv" ""
  ) else if res = 0 then (
    raise End_of_file
  ) else (
    res
  )

let writev_single ?file_offset fd bufs =
  let file_offset =
    match file_offset with
    | Some x -> x
    | None -> uring_file_offset fd
  in
  Fd.use_exn "writev" fd @@ fun fd ->
  let res = Sched.enter (enqueue_writev (file_offset, fd, bufs)) in
  if res < 0 then (
    raise @@ Err.wrap (Uring.error_of_errno res) "writev" ""
  ) else (
    res
  )

let rec writev ?file_offset fd bufs =
  let bytes_written = writev_single ?file_offset fd bufs in
  match Cstruct.shiftv bufs bytes_written with
  | [] -> ()
  | bufs ->
    let file_offset =
      let module I63 = Optint.Int63 in
      match file_offset with
      | None -> None
      | Some ofs when ofs = I63.minus_one -> Some I63.minus_one
      | Some ofs -> Some (I63.add ofs (I63.of_int bytes_written))
    in
    writev ?file_offset fd bufs

let await_readable fd =
  Fd.use_exn "await_readable" fd @@ fun fd ->
  let res = Sched.enter (Sched.enqueue_poll_add fd (Uring.Poll_mask.(pollin + pollerr))) in
  if res < 0 then (
    raise (Err.unclassified (Eio_unix.Unix_error (Uring.error_of_errno res, "await_readable", "")))
  )

let await_writable fd =
  Fd.use_exn "await_writable" fd @@ fun fd ->
  let res = Sched.enter (Sched.enqueue_poll_add fd (Uring.Poll_mask.(pollout + pollerr))) in
  if res < 0 then (
    raise (Err.unclassified (Eio_unix.Unix_error (Uring.error_of_errno res, "await_writable", "")))
  )

let write ?file_offset:off fd buf len =
  let off = file_offset fd off in
  Fd.use_exn "write" fd @@ fun fd ->
  let res = Sched.enter (fun t k -> enqueue_write t k (off, fd, buf, Exactly len)) in
  if res < 0 then (
    raise @@ Err.wrap (Uring.error_of_errno res) "write" ""
  )

let alloc_fixed () = Effect.perform Sched.Alloc

let alloc_fixed_or_wait () = Effect.perform Sched.Alloc_or_wait

let free_fixed buf = Effect.perform (Sched.Free buf)

let splice src ~dst ~len =
  Fd.use_exn "splice-src" src @@ fun src ->
  Fd.use_exn "splice-dst" dst @@ fun dst ->
  let res = Sched.enter (enqueue_splice ~src ~dst ~len) in
  if res > 0 then res
  else if res = 0 then raise End_of_file
  else raise @@ Err.wrap (Uring.error_of_errno res) "splice" ""

let connect fd addr =
  Fd.use_exn "connect" fd @@ fun fd ->
  let res = Sched.enter (enqueue_connect fd addr) in
  if res < 0 then (
    let ex =
      match addr with
      | ADDR_UNIX _ -> Err.wrap_fs (Uring.error_of_errno res) "connect" ""
      | ADDR_INET _ -> Err.wrap (Uring.error_of_errno res) "connect" ""
    in
    raise ex
  )

let send_msg fd ?(fds=[]) ?dst buf =
  Fd.use_exn "send_msg" fd @@ fun fd ->
  Fd.use_exn_list "send_msg" fds @@ fun fds ->
  let res = Sched.enter (enqueue_send_msg fd ~fds ~dst buf) in
  if res < 0 then (
    raise @@ Err.wrap (Uring.error_of_errno res) "send_msg" ""
  ) else res

let recv_msg fd buf =
  Fd.use_exn "recv_msg" fd @@ fun fd ->
  let addr = Uring.Sockaddr.create () in
  let msghdr = Uring.Msghdr.create ~addr buf in
  let res = Sched.enter (enqueue_recv_msg fd msghdr) in
  if res < 0 then (
    raise @@ Err.wrap (Uring.error_of_errno res) "recv_msg" ""
  );
  addr, res

let recv_msg_with_fds ~sw ~max_fds fd buf =
  Fd.use_exn "recv_msg_with_fds" fd @@ fun fd ->
  let addr = Uring.Sockaddr.create () in
  let msghdr = Uring.Msghdr.create ~n_fds:max_fds ~addr buf in
  let res = Sched.enter (enqueue_recv_msg fd msghdr) in
  if res < 0 then (
    raise @@ Err.wrap (Uring.error_of_errno res) "recv_msg" ""
  );
  let fds =
    Uring.Msghdr.get_fds msghdr
    |> List.map (fun fd -> Fd.of_unix ~sw ~close_unix:true fd)
  in
  addr, res, fds

let with_chunk ~fallback fn =
  match alloc_fixed () with
  | Some chunk ->
    Fun.protect ~finally:(fun () -> free_fixed chunk) @@ fun () ->
    fn chunk
  | None ->
    fallback ()

let openat2 ~sw ?seekable ~access ~flags ~perm ~resolve ?dir path =
  let use dir =
    let res = Sched.enter (enqueue_openat2 (access, flags, perm, resolve, dir, path)) in
    if res < 0 then (
      Switch.check sw;    (* If cancelled, report that instead. *)
      raise @@ Err.wrap_fs (Uring.error_of_errno res) "openat2" ""
    );
    let fd : Unix.file_descr = Obj.magic res in
    Fd.of_unix ~sw ?seekable ~close_unix:true fd
  in
  match dir with
  | None -> use None
  | Some dir -> Fd.use_exn "openat2" dir (fun x -> use (Some x))

let openat ~sw ?seekable ~access ~flags ~perm dir path =
  match dir with
  | FD dir -> openat2 ~sw ?seekable ~access ~flags ~perm ~resolve:Uring.Resolve.beneath ~dir path
  | Cwd -> openat2 ~sw ?seekable ~access ~flags ~perm ~resolve:Uring.Resolve.beneath path
  | Fs -> openat2 ~sw ?seekable ~access ~flags ~perm ~resolve:Uring.Resolve.empty path

let fstat t =
  (* todo: use uring  *)
  try
    let ust = Fd.use_exn "fstat" t Unix.LargeFile.fstat in
    let st_kind : Eio.File.Stat.kind =
      match ust.st_kind with
      | Unix.S_REG  -> `Regular_file
      | Unix.S_DIR  -> `Directory
      | Unix.S_CHR  -> `Character_special
      | Unix.S_BLK  -> `Block_device
      | Unix.S_LNK  -> `Symbolic_link
      | Unix.S_FIFO -> `Fifo
      | Unix.S_SOCK -> `Socket
    in
    Eio.File.Stat.{
      dev     = ust.st_dev   |> Int64.of_int;
      ino     = ust.st_ino   |> Int64.of_int;
      kind    = st_kind;
      perm    = ust.st_perm;
      nlink   = ust.st_nlink |> Int64.of_int;
      uid     = ust.st_uid   |> Int64.of_int;
      gid     = ust.st_gid   |> Int64.of_int;
      rdev    = ust.st_rdev  |> Int64.of_int;
      size    = ust.st_size  |> Optint.Int63.of_int64;
      atime   = ust.st_atime;
      mtime   = ust.st_mtime;
      ctime   = ust.st_ctime;
    }
  with Unix.Unix_error (code, name, arg) -> raise @@ Err.wrap_fs code name arg

external eio_mkdirat : Unix.file_descr -> string -> Unix.file_perm -> unit = "caml_eio_mkdirat"

external eio_renameat : Unix.file_descr -> string -> Unix.file_descr -> string -> unit = "caml_eio_renameat"

external eio_getrandom : Cstruct.buffer -> int -> int -> int = "caml_eio_getrandom"

external eio_getdents : Unix.file_descr -> string list = "caml_eio_getdents"

let getrandom { Cstruct.buffer; off; len } =
  let rec loop n =
    if n = len then
      ()
    else
      loop (n + eio_getrandom buffer (off + n) (len - n))
  in
  loop 0

(* [with_parent_dir dir path fn] runs [fn parent (basename path)],
   where [parent] is a path FD for [path]'s parent, resolved using [Resolve.beneath]. *)
let with_parent_dir op dir path fn =
  let dir_path = Filename.dirname path in
  let leaf = Filename.basename path in
  Switch.run (fun sw ->
      let parent =
        match dir with
        | FD d when dir_path = "." -> d
        | _ ->
          openat ~sw ~seekable:false dir dir_path
            ~access:`R
            ~flags:Uring.Open_flags.(cloexec + path + directory)
            ~perm:0
      in
      Fd.use_exn op parent @@ fun parent ->
      fn parent leaf
    )

let mkdir_beneath ~perm dir path =
  (* [mkdir] is really an operation on [path]'s parent. Get a reference to that first: *)
  with_parent_dir "mkdir" dir path @@ fun parent leaf ->
  try eio_mkdirat parent leaf perm
  with Unix.Unix_error (code, name, arg) -> raise @@ Err.wrap_fs code name arg

let unlink ~rmdir dir path =
  (* [unlink] is really an operation on [path]'s parent. Get a reference to that first: *)
  with_parent_dir "unlink" dir path @@ fun parent leaf ->
  let res = Sched.enter (enqueue_unlink (rmdir, parent, leaf)) in
  if res <> 0 then raise @@ Err.wrap_fs (Uring.error_of_errno res) "unlinkat" ""

let rename old_dir old_path new_dir new_path =
  with_parent_dir "renameat-old" old_dir old_path @@ fun old_parent old_leaf ->
  with_parent_dir "renameat-new" new_dir new_path @@ fun new_parent new_leaf ->
  try
    eio_renameat
      old_parent old_leaf
      new_parent new_leaf
  with Unix.Unix_error (code, name, arg) -> raise @@ Err.wrap_fs code name arg

let shutdown socket command =
  Fd.use_exn "shutdown" socket @@ fun fd ->
  try Unix.shutdown fd command
  with Unix.Unix_error (code, name, arg) -> raise @@ Err.wrap code name arg

let accept ~sw fd =
  Ctf.label "accept";
  Fd.use_exn "accept" fd @@ fun fd ->
  let client_addr = Uring.Sockaddr.create () in
  let res = Sched.enter (enqueue_accept fd client_addr) in
  if res < 0 then (
    raise @@ Err.wrap (Uring.error_of_errno res) "accept" ""
  ) else (
    let unix : Unix.file_descr = Obj.magic res in
    let client = Fd.of_unix ~sw ~seekable:false ~close_unix:true unix in
    let client_addr = Uring.Sockaddr.get client_addr in
    client, client_addr
  )

let open_dir ~sw dir path =
  openat ~sw ~seekable:false dir path
    ~access:`R
    ~flags:Uring.Open_flags.(cloexec + directory)
    ~perm:0

let read_dir fd =
  Fd.use_exn "read_dir" fd @@ fun fd ->
  let rec read_all acc fd =
    match eio_getdents fd with
    | [] -> acc
    | files ->
      let files = List.filter (function ".." | "." -> false | _ -> true) files in
      read_all (files @ acc) fd
  in
  Eio_unix.run_in_systhread (fun () -> read_all [] fd)

(* https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml *)
let getaddrinfo ~service node =
  let to_eio_sockaddr_t {Unix.ai_family; ai_addr; ai_socktype; ai_protocol; _ } =
    match ai_family, ai_socktype, ai_addr with
    | (Unix.PF_INET | PF_INET6),
      (Unix.SOCK_STREAM | SOCK_DGRAM),
      Unix.ADDR_INET (inet_addr,port) -> (
        match ai_protocol with
        | 6 -> Some (`Tcp (Eio_unix.Net.Ipaddr.of_unix inet_addr, port))
        | 17 -> Some (`Udp (Eio_unix.Net.Ipaddr.of_unix inet_addr, port))
        | _ -> None)
    | _ -> None
  in
  Eio_unix.run_in_systhread @@ fun () ->
  Unix.getaddrinfo node service []
  |> List.filter_map to_eio_sockaddr_t

let pipe ~sw =
  let unix_r, unix_w = Unix.pipe ~cloexec:true () in
  let r = Fd.of_unix ~sw ~seekable:false ~close_unix:true unix_r in
  let w = Fd.of_unix ~sw ~seekable:false ~close_unix:true unix_w in
  (* See issue #319, PR #327 *)
  Unix.set_nonblock unix_r;
  Unix.set_nonblock unix_w;
  (r, w)

let with_pipe fn =
  Switch.run @@ fun sw ->
  let r, w = pipe ~sw in
  fn r w

module Process = struct
  module Rcfd = Eio_unix.Private.Rcfd

  external eio_spawn :
    Unix.file_descr ->
    Eio_unix.Private.Fork_action.c_action list ->
    int * Unix.file_descr = "caml_eio_clone3"

  external pidfd_send_signal : Unix.file_descr -> int -> unit = "caml_eio_pidfd_send_signal"

  type t = {
    pid : int;
    pid_fd : Fd.t;
    exit_status : Unix.process_status Promise.t;
  }

  let exit_status t = t.exit_status
  let pid t = t.pid

  module Fork_action = Eio_unix.Private.Fork_action

  (* Read a (typically short) error message from a child process. *)
  let rec read_response fd =
    let buf = Cstruct.create 256 in
    match readv fd [buf] with
    | len -> Cstruct.to_string buf ~len ^ read_response fd
    | exception End_of_file -> ""

  let signal t signum =
    Fd.use t.pid_fd ~if_closed:Fun.id @@ fun pid_fd ->
    pidfd_send_signal pid_fd signum

  let rec waitpid pid =
    match Unix.waitpid [] pid with
    | p, status -> assert (p = pid); status
    | exception Unix.Unix_error (EINTR, _, _) -> waitpid pid

  let spawn ~sw actions =
    with_pipe @@ fun errors_r errors_w ->
    Eio_unix.Private.Fork_action.with_actions actions @@ fun c_actions ->
    Switch.check sw;
    let exit_status, set_exit_status = Promise.create () in
    let t =
      Fd.use_exn "errors-w" errors_w @@ fun errors_w ->
      let pid, pid_fd = eio_spawn errors_w c_actions in
      let pid_fd = Fd.of_unix ~sw ~seekable:false ~close_unix:true pid_fd in
      { pid; pid_fd; exit_status }
    in
    Fd.close errors_w;
    Fiber.fork_daemon ~sw (fun () ->
        let cleanup () =
          Fd.close t.pid_fd;
          Promise.resolve set_exit_status (waitpid t.pid);
          `Stop_daemon
        in
        match await_readable t.pid_fd with
        | () -> Eio.Cancel.protect cleanup
        | exception Eio.Cancel.Cancelled _ ->
          Eio.Cancel.protect (fun () ->
              signal t Sys.sigkill;
              await_readable t.pid_fd;
              cleanup ()
            )
      );
    (* Check for errors starting the process. *)
    match read_response errors_r with
    | "" -> t                       (* Success! Execing the child closed [errors_w] and we got EOF. *)
    | err -> failwith err
end
