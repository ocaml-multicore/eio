[@@@alert "-unstable"]

open Eio.Std

module Trace = Eio.Private.Trace
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
  Sched.submit_rw_req st req

let rec enqueue_writev args st action =
  let (file_offset,fd,bufs) = args in
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.writev st.uring ~file_offset fd bufs (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_writev args st action) st.io_q

let enqueue_write st action (file_offset,fd,buf,len) =
  let req = { Sched.op=`W; file_offset; len; fd; cur_off = 0; buf; action } in
  Sched.submit_rw_req st req

let rec enqueue_splice ~src ~dst ~len st action =
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.splice st.uring (Job action) ~src ~dst ~len
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_splice ~src ~dst ~len st action) st.io_q

let rec enqueue_openat2 ((access, flags, perm, resolve, fd, path) as args) st action =
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.openat2 st.uring ~access ~flags ~perm ~resolve ?fd path (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_openat2 args st action) st.io_q

let rec enqueue_statx ((fd, path, buf, flags, mask) as args) st action =
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.statx st.uring ?fd ~mask path buf flags (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_statx args st action) st.io_q

let rec enqueue_unlink ((dir, fd, path) as args) st action =
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.unlink st.uring ~dir ~fd path (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_unlink args st action) st.io_q

let rec enqueue_connect fd addr st action =
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.connect st.uring fd addr (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_connect fd addr st action) st.io_q

let rec enqueue_send_msg fd ~fds ~dst buf st action =
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.send_msg st.uring fd ~fds ?dst buf (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_send_msg fd ~fds ~dst buf st action) st.io_q

let rec enqueue_recv_msg fd msghdr st action =
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.recv_msg st.uring fd msghdr (Job action);
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_recv_msg fd msghdr st action) st.io_q

let rec enqueue_accept fd client_addr st action =
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.accept st.uring fd client_addr (Job action)
    ) in
  if retry then (
    (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_accept fd client_addr st action) st.io_q
  )

let rec enqueue_noop t action =
  let job = Sched.enqueue_job t (fun () -> Uring.noop t.uring (Job_no_cancel action)) in
  if job = None then (
    (* wait until an sqe is available *)
    Queue.push (fun t -> enqueue_noop t action) t.io_q
  )

let noop () =
  let result = Sched.enter "noop" enqueue_noop in
  if result <> 0 then raise (Err.unclassified (Eio_unix.Unix_error (Uring.error_of_errno result, "noop", "")))

let sleep_until time =
  Sched.enter "sleep" @@ fun t k ->
  let job = Eio_utils.Zzz.add t.sleep_q time (Fiber k) in
  Eio.Private.Fiber_context.set_cancel_fn k.fiber (fun ex ->
      Eio_utils.Zzz.remove t.sleep_q job;
      Sched.enqueue_failed_thread t k ex
    )

let read ?file_offset:off fd buf amount =
  let off = file_offset fd off in
  Fd.use_exn "read" fd @@ fun fd ->
  let res = Sched.enter "read" (fun t k -> enqueue_read t k (off, fd, buf, amount)) in
  if res < 0 then (
    raise @@ Err.wrap (Uring.error_of_errno res) "read" ""
  ) else res

let read_exactly ?file_offset fd buf len =
  ignore (read ?file_offset fd buf (Exactly len) : int)

let read_upto ?file_offset fd buf len =
  read ?file_offset fd buf (Upto len)

let rec enqueue_readv args st action =
  let (file_offset,fd,bufs) = args in
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
  let res = Sched.enter "readv" (enqueue_readv (file_offset, fd, bufs)) in
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
  let res = Sched.enter "writev" (enqueue_writev (file_offset, fd, bufs)) in
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
  let res = Sched.enter "await_readable" (Sched.enqueue_poll_add fd (Uring.Poll_mask.(pollin + pollerr))) in
  if res < 0 then (
    raise (Err.unclassified (Eio_unix.Unix_error (Uring.error_of_errno res, "await_readable", "")))
  )

let await_writable fd =
  Fd.use_exn "await_writable" fd @@ fun fd ->
  let res = Sched.enter "await_writable" (Sched.enqueue_poll_add fd (Uring.Poll_mask.(pollout + pollerr))) in
  if res < 0 then (
    raise (Err.unclassified (Eio_unix.Unix_error (Uring.error_of_errno res, "await_writable", "")))
  )

let write ?file_offset:off fd buf len =
  let off = file_offset fd off in
  Fd.use_exn "write" fd @@ fun fd ->
  let res = Sched.enter "write" (fun t k -> enqueue_write t k (off, fd, buf, Exactly len)) in
  if res < 0 then (
    raise @@ Err.wrap (Uring.error_of_errno res) "write" ""
  )

let alloc_fixed () =
  let s = Sched.get () in
  match s.mem with
  | None -> None
  | Some mem ->
    match Uring.Region.alloc mem with
    | buf -> Some buf
    | exception Uring.Region.No_space -> None

let alloc_fixed_or_wait () =
  let s = Sched.get () in
  match s.mem with
  | None -> failwith "No fixed buffer available"
  | Some mem ->
    match Uring.Region.alloc mem with
    | buf -> buf
    | exception Uring.Region.No_space ->
      let id = Eio.Private.Trace.mint_id () in
      let trigger = Eio.Private.Single_waiter.create () in
      let node = Lwt_dllist.add_r trigger s.mem_q in
      try
        Eio.Private.Single_waiter.await trigger "alloc_fixed_or_wait" id
      with ex ->
        Lwt_dllist.remove node;
        raise ex

let rec free_fixed buf =
  let s = Sched.get () in
  match Lwt_dllist.take_opt_l s.mem_q with
  | None -> Uring.Region.free buf
  | Some k ->
    if not (Eio.Private.Single_waiter.wake k (Ok buf)) then
      free_fixed buf    (* [k] was already cancelled, but not yet removed from the queue *)

let splice src ~dst ~len =
  Fd.use_exn "splice-src" src @@ fun src ->
  Fd.use_exn "splice-dst" dst @@ fun dst ->
  let res = Sched.enter "splice" (enqueue_splice ~src ~dst ~len) in
  if res > 0 then res
  else if res = 0 then raise End_of_file
  else raise @@ Err.wrap (Uring.error_of_errno res) "splice" ""

let connect fd addr =
  Fd.use_exn "connect" fd @@ fun fd ->
  let res = Sched.enter "connect" (enqueue_connect fd addr) in
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
  let res = Sched.enter "send_msg" (enqueue_send_msg fd ~fds ~dst buf) in
  if res < 0 then (
    raise @@ Err.wrap (Uring.error_of_errno res) "send_msg" ""
  ) else res

let recv_msg fd buf =
  Fd.use_exn "recv_msg" fd @@ fun fd ->
  let addr = Uring.Sockaddr.create () in
  let msghdr = Uring.Msghdr.create ~addr buf in
  let res = Sched.enter "recv_msg" (enqueue_recv_msg fd msghdr) in
  if res < 0 then (
    raise @@ Err.wrap (Uring.error_of_errno res) "recv_msg" ""
  );
  addr, res

let recv_msg_with_fds ~sw ~max_fds fd buf =
  Fd.use_exn "recv_msg_with_fds" fd @@ fun fd ->
  let addr = Uring.Sockaddr.create () in
  let msghdr = Uring.Msghdr.create ~n_fds:max_fds ~addr buf in
  let res = Sched.enter "recv_msg_with_fds" (enqueue_recv_msg fd msghdr) in
  if res < 0 then (
    raise @@ Err.wrap (Uring.error_of_errno res) "recv_msg" ""
  );
  let fds = Uring.Msghdr.get_fds msghdr |> Fd.of_unix_list ~sw in
  addr, res, fds

let with_chunk ~fallback fn =
  match alloc_fixed () with
  | Some chunk ->
    Fun.protect ~finally:(fun () -> free_fixed chunk) @@ fun () ->
    fn chunk
  | None ->
    fallback ()

let rec openat2 ~sw ?seekable ~access ~flags ~perm ~resolve ?dir path =
  let use dir_opt =
    let res = Sched.enter "openat2" (enqueue_openat2 (access, flags, perm, resolve, dir_opt, path)) in
    if res < 0 then (
      Switch.check sw;    (* If cancelled, report that instead. *)
      match Uring.error_of_errno res with
      | EAGAIN ->
        (* Linux can return this due to a concurrent update.
           It also seems to happen sometimes with no concurrent updates. *)
        openat2 ~sw ?seekable ~access ~flags ~perm ~resolve ?dir path
      | e -> raise @@ Err.wrap_fs e "openat2" ""
    ) else (
      let fd : Unix.file_descr = Obj.magic res in
      Fd.of_unix ~sw ?seekable ~close_unix:true fd
    )
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

external eio_symlinkat : string -> Unix.file_descr -> string -> unit = "caml_eio_symlinkat"

external eio_getrandom : Cstruct.buffer -> int -> int -> int = "caml_eio_getrandom"

external eio_getdents : Unix.file_descr -> string list = "caml_eio_getdents"

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
  (* todo: https://github.com/ocaml-multicore/ocaml-uring/pull/103 *)
  Eio_unix.run_in_systhread ~label:"fsync" @@ fun () ->
  Fd.use_exn "fsync" fd Unix.fsync

let ftruncate fd len =
  Eio_unix.run_in_systhread ~label:"ftruncate" @@ fun () ->
  Fd.use_exn "ftruncate" fd @@ fun fd ->
  Unix.LargeFile.ftruncate fd (Optint.Int63.to_int64 len)

let getrandom { Cstruct.buffer; off; len } =
  let rec loop n =
    if n = len then
      ()
    else
      loop (n + eio_getrandom buffer (off + n) (len - n))
  in
  loop 0

(* [with_parent_dir_fd dir path fn] runs [fn parent (basename path)],
   where [parent] is a path FD for [path]'s parent, resolved using [Resolve.beneath].

   If [basename path] is ".." then we treat it as if path had "/." on the end,
   to avoid the special case.

   todo: Optimise this by doing [fn AT_FDCWD path] if [dir = Fs].
*)
let with_parent_dir_fd dir path fn =
  let dir_path = Filename.dirname path in
  let leaf = Filename.basename path in
  Switch.run ~name:"with_parent_dir" (fun sw ->
      match dir with
      | _ when leaf = ".." ->
        let fd =
          openat ~sw ~seekable:false dir path   (* Open the full path *)
            ~access:`R
            ~flags:Uring.Open_flags.(cloexec + path + directory)
            ~perm:0
        in
        fn fd "."
      | FD d when dir_path = "." -> fn d leaf
      | _ ->
        let parent =
          openat ~sw ~seekable:false dir dir_path
            ~access:`R
            ~flags:Uring.Open_flags.(cloexec + path + directory)
            ~perm:0
        in
        fn parent leaf
    )

let with_parent_dir op dir path fn =
  with_parent_dir_fd dir path @@ fun parent leaf ->
  Fd.use_exn op parent @@ fun parent ->
  fn parent leaf

let statx_raw ?fd ~mask path buf flags =
  let res =
    match fd with
    | None -> Sched.enter "statx" (enqueue_statx (None, path, buf, flags, mask)) 
    | Some fd ->
      Fd.use_exn "statx" fd @@ fun fd ->
      Sched.enter "statx" (enqueue_statx (Some fd, path, buf, flags, mask))
  in
  if res <> 0 then raise @@ Err.wrap_fs (Uring.error_of_errno res) "statx" path

let statx ~mask ~follow fd path buf =
  let module X = Uring.Statx in
  let flags = if follow then X.Flags.empty_path else X.Flags.(empty_path + symlink_nofollow) in
  match fd with
  | Fs -> statx_raw ~mask path buf flags
  | FD fd when path = "" -> statx_raw ~fd ~mask "" buf flags
  | Cwd | FD _ when not follow ->
    with_parent_dir_fd fd path @@ fun parent leaf ->
    statx_raw ~mask ~fd:parent leaf buf flags
  | Cwd | FD _ ->
    Switch.run ~name:"statx" @@ fun sw ->
    let fd = openat ~sw ~seekable:false fd (if path = "" then "." else path)
        ~access:`R
        ~flags:Uring.Open_flags.(cloexec + path)
        ~perm:0
    in
    statx_raw ~fd ~mask "" buf flags

let mkdir ~perm dir path =
  (* [mkdir] is really an operation on [path]'s parent. Get a reference to that first: *)
  with_parent_dir "mkdir" dir path @@ fun parent leaf ->
  try eio_mkdirat parent leaf perm
  with Unix.Unix_error (code, name, arg) -> raise @@ Err.wrap_fs code name arg

let unlink ~rmdir dir path =
  (* [unlink] is really an operation on [path]'s parent. Get a reference to that first: *)
  with_parent_dir "unlink" dir path @@ fun parent leaf ->
  let res = Sched.enter "unlink" (enqueue_unlink (rmdir, parent, leaf)) in
  if res <> 0 then raise @@ Err.wrap_fs (Uring.error_of_errno res) "unlinkat" ""

let rename old_dir old_path new_dir new_path =
  with_parent_dir "renameat-old" old_dir old_path @@ fun old_parent old_leaf ->
  with_parent_dir "renameat-new" new_dir new_path @@ fun new_parent new_leaf ->
  try
    eio_renameat
      old_parent old_leaf
      new_parent new_leaf
  with Unix.Unix_error (code, name, arg) -> raise @@ Err.wrap_fs code name arg

let symlink ~link_to dir path =
  with_parent_dir "symlinkat-new" dir path @@ fun parent leaf ->
  try
    eio_symlinkat link_to parent leaf
  with Unix.Unix_error (code, name, arg) -> raise @@ Err.wrap_fs code name arg

let shutdown socket command =
  try
    Fd.use_exn "shutdown" socket @@ fun fd ->
    Unix.shutdown fd command
  with
  | Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()
  | Unix.Unix_error (code, name, arg) -> raise @@ Err.wrap code name arg

let accept ~sw fd =
  Fd.use_exn "accept" fd @@ fun fd ->
  let client_addr = Uring.Sockaddr.create () in
  let res = Sched.enter "accept" (enqueue_accept fd client_addr) in
  if res < 0 then (
    raise @@ Err.wrap (Uring.error_of_errno res) "accept" ""
  ) else (
    let unix : Unix.file_descr = Obj.magic res in
    let client = Fd.of_unix ~sw ~seekable:false ~close_unix:true unix in
    let client_addr = Uring.Sockaddr.get client_addr in
    client, client_addr
  )

let read_dir fd =
  Fd.use_exn "read_dir" fd @@ fun fd ->
  let rec read_all acc fd =
    match eio_getdents fd with
    | [] -> acc
    | files ->
      let files = List.filter (function ".." | "." -> false | _ -> true) files in
      read_all (files @ acc) fd
  in
  Eio_unix.run_in_systhread ~label:"read_dir" (fun () -> read_all [] fd)

let read_link fd path =
  try
    with_parent_dir_fd fd path @@ fun parent leaf ->
    Eio_unix.run_in_systhread ~label:"read_link" (fun () -> Eio_unix.Private.read_link (Some parent) leaf)
  with Unix.Unix_error (code, name, arg) -> raise @@ Err.wrap_fs code name arg

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
  Eio_unix.run_in_systhread ~label:"getaddrinfo" @@ fun () ->
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
  Switch.run ~name:"with_pipe" @@ fun sw ->
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
      let pid, pid_fd =
        Eio.Private.Trace.with_span "spawn" @@ fun () ->
        eio_spawn errors_w c_actions
      in
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

module Sockopt = struct
  let tcp_maxseg = 2          (* TCP_MAXSEG from netinet/tcp.h *)
  let tcp_cork = 3           (* TCP_CORK from netinet/tcp.h *)
  let tcp_keepidle = 4       (* TCP_KEEPIDLE *)
  let tcp_keepintvl = 5      (* TCP_KEEPINTVL *)
  let tcp_keepcnt = 6        (* TCP_KEEPCNT *)
  let tcp_syncnt = 7         (* TCP_SYNCNT *)
  let tcp_linger2 = 8        (* TCP_LINGER2 *)
  let tcp_defer_accept = 9   (* TCP_DEFER_ACCEPT *)
  let tcp_window_clamp = 10  (* TCP_WINDOW_CLAMP *)
  let tcp_quickack = 12      (* TCP_QUICKACK *)
  let tcp_congestion = 13    (* TCP_CONGESTION *)
  let tcp_user_timeout = 18  (* TCP_USER_TIMEOUT *)
  let tcp_fastopen = 23      (* TCP_FASTOPEN *)
  let ipproto_tcp = 6        (* IPPROTO_TCP *)
  let ipproto_ip = 0         (* IPPROTO_IP *)
  let ip_freebind = 15       (* IP_FREEBIND *)
  let ip_bind_address_no_port = 24  (* IP_BIND_ADDRESS_NO_PORT *)
  let ip_local_port_range = 51      (* IP_LOCAL_PORT_RANGE *)
  let ip_ttl = 2             (* IP_TTL *)
  let ip_mtu = 14            (* IP_MTU *)
  let ip_mtu_discover = 10   (* IP_MTU_DISCOVER *)

  external setsockopt_int : Unix.file_descr -> int -> int -> int -> unit = "caml_eio_sockopt_int_set"
  external getsockopt_int : Unix.file_descr -> int -> int -> int = "caml_eio_sockopt_int_get"
  external setsockopt_string : Unix.file_descr -> int -> int -> string -> unit = "caml_eio_sockopt_string_set"
  external getsockopt_string : Unix.file_descr -> int -> int -> string = "caml_eio_sockopt_string_get"

  (* Define Linux-specific socket options as extensions of Eio.Net.Sockopt.t *)
  type _ Eio.Net.Sockopt.t +=
    | TCP_CORK : bool Eio.Net.Sockopt.t
    | TCP_KEEPIDLE : int Eio.Net.Sockopt.t
    | TCP_KEEPINTVL : int Eio.Net.Sockopt.t
    | TCP_KEEPCNT : int Eio.Net.Sockopt.t
    | TCP_USER_TIMEOUT : int Eio.Net.Sockopt.t
    | TCP_MAXSEG : int Eio.Net.Sockopt.t
    | TCP_LINGER2 : int option Eio.Net.Sockopt.t
    | TCP_DEFER_ACCEPT : int Eio.Net.Sockopt.t
    | TCP_CONGESTION : string Eio.Net.Sockopt.t
    | TCP_SYNCNT : int Eio.Net.Sockopt.t
    | TCP_WINDOW_CLAMP : int Eio.Net.Sockopt.t
    | TCP_QUICKACK : bool Eio.Net.Sockopt.t
    | TCP_FASTOPEN : int Eio.Net.Sockopt.t
    | IP_FREEBIND : bool Eio.Net.Sockopt.t
    | IP_BIND_ADDRESS_NO_PORT : bool Eio.Net.Sockopt.t
    | IP_LOCAL_PORT_RANGE : (int * int) Eio.Net.Sockopt.t
    | IP_TTL : int Eio.Net.Sockopt.t
    | IP_MTU : int Eio.Net.Sockopt.t
    | IP_MTU_DISCOVER : [`Want | `Dont | `Do | `Probe] Eio.Net.Sockopt.t

  let pp : type a. a Eio.Net.Sockopt.t -> Format.formatter -> a -> unit = fun opt f v ->
    match opt with
    | TCP_CORK -> Fmt.pf f "TCP_CORK = %b" v
    | TCP_KEEPIDLE -> Fmt.pf f "TCP_KEEPIDLE = %d" v
    | TCP_KEEPINTVL -> Fmt.pf f "TCP_KEEPINTVL = %d" v
    | TCP_KEEPCNT -> Fmt.pf f "TCP_KEEPCNT = %d" v
    | TCP_USER_TIMEOUT -> Fmt.pf f "TCP_USER_TIMEOUT = %d" v
    | TCP_MAXSEG -> Fmt.pf f "TCP_MAXSEG = %d" v
    | TCP_LINGER2 -> Fmt.(pf f "TCP_LINGER2 = %a" (option ~none:(any "<none>") int) v)
    | TCP_DEFER_ACCEPT -> Fmt.pf f "TCP_DEFER_ACCEPT = %d" v
    | TCP_CONGESTION -> Fmt.pf f "TCP_CONGESTION = %s" v
    | TCP_SYNCNT -> Fmt.pf f "TCP_SYNCNT = %d" v
    | TCP_WINDOW_CLAMP -> Fmt.pf f "TCP_WINDOW_CLAMP = %d" v
    | TCP_QUICKACK -> Fmt.pf f "TCP_QUICKACK = %b" v
    | TCP_FASTOPEN -> Fmt.pf f "TCP_FASTOPEN = %d" v
    | IP_FREEBIND -> Fmt.pf f "IP_FREEBIND = %b" v
    | IP_BIND_ADDRESS_NO_PORT -> Fmt.pf f "IP_BIND_ADDRESS_NO_PORT = %b" v
    | IP_LOCAL_PORT_RANGE ->
        let (lower, upper) = v in
        Fmt.pf f "IP_LOCAL_PORT_RANGE = (%d, %d)" lower upper
    | IP_TTL -> Fmt.pf f "IP_TTL = %d" v
    | IP_MTU -> Fmt.pf f "IP_MTU = %d" v
    | IP_MTU_DISCOVER ->
        let s = match v with
          | `Want -> "Want"
          | `Dont -> "Dont"
          | `Do -> "Do"
          | `Probe -> "Probe"
        in
        Fmt.pf f "IP_MTU_DISCOVER = %s" s
    | _ -> Eio_unix.Net.Sockopt.pp opt f v

  let with_fd_set fd fn =
    Fd.use_exn "setsockopt" fd fn

  let with_fd_get fd fn =
    Fd.use_exn "getsockopt" fd fn

  let set : type a. Fd.t -> a Eio.Net.Sockopt.t -> a -> unit = fun fd opt v ->
    match opt with
    | TCP_CORK ->
      with_fd_set fd (fun fd -> setsockopt_int fd ipproto_tcp tcp_cork (if v then 1 else 0))
    | TCP_KEEPIDLE ->
      if v < 0 then
        invalid_arg (Printf.sprintf "TCP_KEEPIDLE must be non-negative, got %d" v);
      with_fd_set fd (fun fd -> setsockopt_int fd ipproto_tcp tcp_keepidle v)
    | TCP_KEEPINTVL ->
      if v < 0 then
        invalid_arg (Printf.sprintf "TCP_KEEPINTVL must be non-negative, got %d" v);
      with_fd_set fd (fun fd -> setsockopt_int fd ipproto_tcp tcp_keepintvl v)
    | TCP_KEEPCNT ->
      if v < 0 then
        invalid_arg (Printf.sprintf "TCP_KEEPCNT must be non-negative, got %d" v);
      with_fd_set fd (fun fd -> setsockopt_int fd ipproto_tcp tcp_keepcnt v)
    | TCP_USER_TIMEOUT ->
      if v < 0 then
        invalid_arg (Printf.sprintf "TCP_USER_TIMEOUT must be non-negative, got %d" v);
      with_fd_set fd (fun fd -> setsockopt_int fd ipproto_tcp tcp_user_timeout v)
    | TCP_MAXSEG ->
      if v < 0 then
        invalid_arg (Printf.sprintf "TCP_MAXSEG must be non-negative, got %d" v);
      with_fd_set fd (fun fd -> setsockopt_int fd ipproto_tcp tcp_maxseg v)
    | TCP_LINGER2 ->
      let v = match v with
        | None -> -1
        | Some n when n < 0 ->
            invalid_arg (Printf.sprintf "TCP_LINGER2 must be non-negative, got %d" n);
        | Some n -> n
      in
      with_fd_set fd (fun fd -> setsockopt_int fd ipproto_tcp tcp_linger2 v)
    | TCP_DEFER_ACCEPT ->
      if v < 0 then
        invalid_arg (Printf.sprintf "TCP_DEFER_ACCEPT must be non-negative, got %d" v);
      with_fd_set fd (fun fd -> setsockopt_int fd ipproto_tcp tcp_defer_accept v)
    | TCP_CONGESTION ->
      with_fd_set fd (fun fd -> setsockopt_string fd ipproto_tcp tcp_congestion v)
    | TCP_SYNCNT ->
      if v < 1 || v > 255 then
        invalid_arg (Printf.sprintf "TCP_SYNCNT must be between 1 and 255, got %d" v);
      with_fd_set fd (fun fd -> setsockopt_int fd ipproto_tcp tcp_syncnt v)
    | TCP_WINDOW_CLAMP ->
      if v < 0 then
        invalid_arg (Printf.sprintf "TCP_WINDOW_CLAMP must be non-negative, got %d" v);
      with_fd_set fd (fun fd -> setsockopt_int fd ipproto_tcp tcp_window_clamp v)
    | TCP_QUICKACK ->
      with_fd_set fd (fun fd -> setsockopt_int fd ipproto_tcp tcp_quickack (if v then 1 else 0))
    | TCP_FASTOPEN ->
      if v < 0 then
        invalid_arg (Printf.sprintf "TCP_FASTOPEN queue length must be non-negative, got %d" v);
      with_fd_set fd (fun fd -> setsockopt_int fd ipproto_tcp tcp_fastopen v)
    | IP_FREEBIND ->
      with_fd_set fd (fun fd -> setsockopt_int fd ipproto_ip ip_freebind (if v then 1 else 0))
    | IP_BIND_ADDRESS_NO_PORT ->
      with_fd_set fd (fun fd -> setsockopt_int fd ipproto_ip ip_bind_address_no_port (if v then 1 else 0))
    | IP_LOCAL_PORT_RANGE ->
      let (lower, upper) = v in
      if lower < 0 || lower > 65535 then
        invalid_arg (Printf.sprintf "IP_LOCAL_PORT_RANGE lower bound must be 0-65535, got %d" lower);
      if upper < 0 || upper > 65535 then
        invalid_arg (Printf.sprintf "IP_LOCAL_PORT_RANGE upper bound must be 0-65535, got %d" upper);
      if lower <> 0 && upper <> 0 && lower > upper then
        invalid_arg (Printf.sprintf "IP_LOCAL_PORT_RANGE lower bound (%d) must be <= upper bound (%d)" lower upper);
      let combined = (upper lsl 16) lor lower in
      with_fd_set fd (fun fd ->
        setsockopt_int fd ipproto_ip ip_local_port_range combined)
    | IP_TTL ->
      if v < 1 || v > 255 then
        invalid_arg (Printf.sprintf "IP_TTL must be between 1 and 255, got %d" v);
      with_fd_set fd (fun fd -> setsockopt_int fd ipproto_ip ip_ttl v)
    | IP_MTU ->
      invalid_arg "IP_MTU is a read-only socket option"
    | IP_MTU_DISCOVER ->
      let i = match v with
        | `Dont -> 0
        | `Want -> 1
        | `Do -> 2
        | `Probe -> 3 in
      with_fd_set fd (fun fd -> setsockopt_int fd ipproto_ip ip_mtu_discover i)
    | _ -> Eio_unix.Net.Sockopt.set fd opt v

  let get : type a. Fd.t -> a Eio.Net.Sockopt.t -> a = fun fd opt ->
    match opt with
    | TCP_CORK ->
      with_fd_get fd (fun fd -> getsockopt_int fd ipproto_tcp tcp_cork <> 0)
    | TCP_KEEPIDLE ->
      with_fd_get fd (fun fd -> getsockopt_int fd ipproto_tcp tcp_keepidle)
    | TCP_KEEPINTVL ->
      with_fd_get fd (fun fd -> getsockopt_int fd ipproto_tcp tcp_keepintvl)
    | TCP_KEEPCNT ->
      with_fd_get fd (fun fd -> getsockopt_int fd ipproto_tcp tcp_keepcnt)
    | TCP_USER_TIMEOUT ->
      with_fd_get fd (fun fd -> getsockopt_int fd ipproto_tcp tcp_user_timeout)
    | TCP_MAXSEG ->
      with_fd_get fd (fun fd -> getsockopt_int fd ipproto_tcp tcp_maxseg)
    | TCP_LINGER2 ->
      let v = with_fd_get fd (fun fd -> getsockopt_int fd ipproto_tcp tcp_linger2) in
      if v = -1 then None else Some v
    | TCP_DEFER_ACCEPT ->
      with_fd_get fd (fun fd -> getsockopt_int fd ipproto_tcp tcp_defer_accept)
    | TCP_CONGESTION ->
      with_fd_get fd (fun fd -> getsockopt_string fd ipproto_tcp tcp_congestion)
    | TCP_SYNCNT ->
      with_fd_get fd (fun fd -> getsockopt_int fd ipproto_tcp tcp_syncnt)
    | TCP_WINDOW_CLAMP ->
      with_fd_get fd (fun fd -> getsockopt_int fd ipproto_tcp tcp_window_clamp)
    | TCP_QUICKACK ->
      with_fd_get fd (fun fd -> getsockopt_int fd ipproto_tcp tcp_quickack <> 0)
    | TCP_FASTOPEN ->
      with_fd_get fd (fun fd -> getsockopt_int fd ipproto_tcp tcp_fastopen)
    | IP_FREEBIND ->
      with_fd_get fd (fun fd -> getsockopt_int fd ipproto_ip ip_freebind <> 0)
    | IP_BIND_ADDRESS_NO_PORT ->
      with_fd_get fd (fun fd -> getsockopt_int fd ipproto_ip ip_bind_address_no_port <> 0)
    | IP_LOCAL_PORT_RANGE ->
      with_fd_get fd (fun fd ->
        let combined = getsockopt_int fd ipproto_ip ip_local_port_range in
        let lower = combined land 0xFFFF in
        let upper = (combined lsr 16) land 0xFFFF in
        lower, upper)
    | IP_TTL ->
      with_fd_get fd (fun fd -> getsockopt_int fd ipproto_ip ip_ttl)
    | IP_MTU ->
      with_fd_get fd (fun fd -> getsockopt_int fd ipproto_ip ip_mtu)
    | IP_MTU_DISCOVER -> begin
      let i = with_fd_get fd (fun fd -> getsockopt_int fd ipproto_ip ip_mtu_discover) in
      match i with
      | 0 (* IP_PMTUDISC_DONT *)  -> `Dont
      | 1 (* IP_PMTUDISC_WANT *)  -> `Want
      | 2 (* IP_PMTUDISC_DO *)    -> `Do
      | 3 (* IP_PMTUDISC_PROBE *) -> `Probe
      | i -> failwith (Printf.sprintf "Unknown IP_MTU_DISCOVER value: %d" i) end
    | _ -> Eio_unix.Net.Sockopt.get fd opt
end
