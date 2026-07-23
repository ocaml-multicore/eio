open Eio.Std

module Trace = Eio.Private.Trace
module Fd = Eio_unix.Fd

module Res = struct
  let unit_result (t : Uring.Res.t) =
    let t = (t :> int) in
    if t = 0 then Ok ()
    else if t < 0 then Error (Uring.error_of_errno t)
    else Error ERANGE (* Result too large *)
end

module Fixed = struct
  include Fixed

  let alloc () =
    let s = Sched.get () in
    match s.mem with
    | None -> None
    | Some mem ->
      match Fixed.alloc mem with
      | buf -> Some buf
      | exception Fixed.No_space -> None

  let alloc_or_wait () =
    let s = Sched.get () in
    match s.mem with
    | None -> failwith "No fixed buffer available"
    | Some mem ->
      match Fixed.alloc mem with
      | buf -> buf
      | exception Fixed.No_space ->
        let id = Eio.Private.Trace.mint_id () in
        let trigger = Eio.Private.Single_waiter.create () in
        let node = Lwt_dllist.add_r trigger s.mem_q in
        try
          Eio.Private.Single_waiter.await trigger "alloc_fixed_or_wait" id
        with ex ->
          Lwt_dllist.remove node;
          raise ex

  let rec free buf =
    let s = Sched.get () in
    match Lwt_dllist.take_opt_l s.mem_q with
    | None -> Fixed.free buf
    | Some k ->
      if not (Eio.Private.Single_waiter.wake k (Ok buf)) then
        free buf    (* [k] was already cancelled, but not yet removed from the queue *)

  let use ~fallback fn =
    match alloc () with
    | Some chunk ->
      Fun.protect ~finally:(fun () -> free chunk) @@ fun () ->
      fn chunk
    | None ->
      fallback ()
end

type dir_fd =
  | FD of Fd.t
  | Cwd         (* Confined to "." *)
  | Fs          (* Unconfined "."; also allows absolute paths *)

let uring_file_offset t =
  if Fd.is_seekable t then Optint.Int63.minus_one else Optint.Int63.zero

let file_offset t = function
  | Some x -> x
  | None when Fd.is_seekable t -> Optint.Int63.minus_one
  | None -> Optint.Int63.zero

let rec enqueue_read st action req =
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      let (file_offset,fd,buf,len) = req in
      let off = Fixed.to_offset buf in
      Uring.read_fixed st.uring ~file_offset fd ~off ~len (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_read st action req) st.io_q

let rec enqueue_writev args st action =
  let (file_offset,fd,bufs) = args in
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.writev st.uring ~file_offset fd bufs (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_writev args st action) st.io_q

let rec enqueue_write st action req =
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      let (file_offset,fd,buf,len) = req in
      let off = Fixed.to_offset buf in
      Uring.write_fixed st.uring ~file_offset fd ~off ~len (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_write st action req) st.io_q

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
  match Res.unit_result @@ Sched.enter "noop" enqueue_noop with
  | Ok () -> ()
  | Error e -> raise (Err.unclassified (Eio_unix.Unix_error (e, "noop", "")))

let sleep_until time =
  Sched.enter "sleep" @@ fun t k ->
  let job = Eio_utils.Zzz.add t.sleep_q time (Fiber k) in
  Eio.Private.Fiber_context.set_cancel_fn k.fiber (fun ex ->
      Eio_utils.Zzz.remove t.sleep_q job;
      Sched.enqueue_failed_thread t k ex
    )

let read_upto ?file_offset:off fd buf amount =
  let off = file_offset fd off in
  Fd.use_exn "read" fd @@ fun fd ->
  let res = Sched.enter "read" (fun t k ->
      enqueue_read t k (off, fd, buf, amount)
    ) in
  match Uring.Res.int_result res with
  | Ok 0 -> raise End_of_file
  | Ok n -> n
  | Error Unix.EIO when Unix.isatty fd -> raise End_of_file
  | Error e -> raise @@ Err.v e "read" ""

let rec read_exactly ?file_offset fd buf len =
  let got = read_upto ?file_offset fd buf len in
  if got < len then (
    let file_offset = Option.map (Optint.Int63.(add (of_int got))) file_offset in
    read_exactly ?file_offset fd (Fixed.shift buf got) (len - got)
  )

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
  match Uring.Res.int_result res with
  | Ok 0 -> raise End_of_file
  | Ok n -> n
  | Error Unix.EIO when Unix.isatty fd -> raise End_of_file
  | Error e -> raise @@ Err.v e "readv" ""

let writev_single ?file_offset fd bufs =
  let file_offset =
    match file_offset with
    | Some x -> x
    | None -> uring_file_offset fd
  in
  Fd.use_exn "writev" fd @@ fun fd ->
  let res = Sched.enter "writev" (enqueue_writev (file_offset, fd, bufs)) in
  match Uring.Res.int_result res with
  | Ok x -> x
  | Error e -> raise @@ Err.v e "writev" ""

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
  match Uring.Res.int_result res with
  | Ok _ -> ()
  | Error e -> raise (Err.unclassified (Eio_unix.Unix_error (e, "await_readable", "")))

let await_writable fd =
  Fd.use_exn "await_writable" fd @@ fun fd ->
  let res = Sched.enter "await_writable" (Sched.enqueue_poll_add fd (Uring.Poll_mask.(pollout + pollerr))) in
  match Uring.Res.int_result res with
  | Ok _ -> ()
  | Error e -> raise (Err.unclassified (Eio_unix.Unix_error (e, "await_writable", "")))

let write_single ?file_offset:off fd buf len =
  let off = file_offset fd off in
  Fd.use_exn "write" fd @@ fun fd ->
  let res = Sched.enter "write" (fun t k ->
      enqueue_write t k (off, fd, buf, len)
    ) in
  match Uring.Res.int_result res with
  | Ok x -> x
  | Error e -> raise @@ Err.v e "write" ""

let rec write ?file_offset fd buf len =
  let wrote = write_single ?file_offset fd buf len in
  if wrote < len then (
    let file_offset = Option.map (Optint.Int63.(add (of_int wrote))) file_offset in
    write ?file_offset fd (Fixed.shift buf wrote) (len - wrote)
  )

let splice src ~dst ~len =
  Fd.use_exn "splice-src" src @@ fun src ->
  Fd.use_exn "splice-dst" dst @@ fun dst ->
  let res = Sched.enter "splice" (enqueue_splice ~src ~dst ~len) in
  match Uring.Res.int_result res with
  | Ok 0 -> raise End_of_file
  | Ok n -> n
  | Error e -> raise @@ Err.v e "splice" ""

let connect fd addr =
  Fd.use_exn "connect" fd @@ fun fd ->
  let res = Sched.enter "connect" (enqueue_connect fd addr) in
  match Res.unit_result res with
  | Ok () -> ()
  | Error e -> raise (Err.v e "connect" "")

let send_msg fd ?(fds=[]) ?dst buf =
  Fd.use_exn "send_msg" fd @@ fun fd ->
  Fd.use_exn_list "send_msg" fds @@ fun fds ->
  let res = Sched.enter "send_msg" (enqueue_send_msg fd ~fds ~dst buf) in
  match Uring.Res.int_result res with
  | Ok x -> x
  | Error e -> raise @@ Err.v e "send_msg" ""

let recv_msg fd buf =
  Fd.use_exn "recv_msg" fd @@ fun fd ->
  let addr = Uring.Sockaddr.create () in
  let msghdr = Uring.Msghdr.create ~addr buf in
  let res = Sched.enter "recv_msg" (enqueue_recv_msg fd msghdr) in
  match Uring.Res.int_result res with
  | Error e -> raise @@ Err.v e "recv_msg" ""
  | Ok res -> addr, res

let recv_msg_with_fds ~sw ~max_fds fd buf =
  Fd.use_exn "recv_msg_with_fds" fd @@ fun fd ->
  let addr = Uring.Sockaddr.create () in
  let msghdr = Uring.Msghdr.create ~n_fds:max_fds ~addr buf in
  let res = Sched.enter "recv_msg_with_fds" (enqueue_recv_msg fd msghdr) in
  match Uring.Res.int_result res with
  | Error e -> raise @@ Err.v e "recv_msg" ""
  | Ok res ->
    let fds = Uring.Msghdr.get_fds msghdr |> Fd.of_unix_list ~sw in
    addr, res, fds

let rec openat2 ~sw ?seekable ~access ~flags ~perm ~resolve ?dir path =
  let use dir_opt =
    let res = Sched.enter "openat2" (enqueue_openat2 (access, flags, perm, resolve, dir_opt, path)) in
    match Uring.Res.fd_result res with
    | Ok fd -> Fd.of_unix ~sw ?seekable ~close_unix:true fd
    | Error e ->
      Switch.check sw;    (* If cancelled, report that instead. *)
      match e with
      | EAGAIN ->
        (* Linux can return this due to a concurrent update.
           It also seems to happen sometimes with no concurrent updates. *)
        openat2 ~sw ?seekable ~access ~flags ~perm ~resolve ?dir path
      | e -> raise @@ Err.v e "openat2" ""
  in
  match dir with
  | None -> use None
  | Some dir -> Fd.use_exn "openat2" dir (fun x -> use (Some x))

let openat ~sw ?seekable ~access ~flags ~perm dir path =
  match dir with
  | FD dir -> openat2 ~sw ?seekable ~access ~flags ~perm ~resolve:Uring.Resolve.beneath ~dir path
  | Cwd -> openat2 ~sw ?seekable ~access ~flags ~perm ~resolve:Uring.Resolve.beneath path
  | Fs -> openat2 ~sw ?seekable ~access ~flags ~perm ~resolve:Uring.Resolve.empty path

let float_of_time s ns =
  let s = Int64.to_float s in
  let f = s +. (float ns /. 1e9) in
  (* It's possible that we might round up to the next second.
     Since some algorithms only care about the seconds part,
     make sure the integer part is always [s]: *)
  if floor f = s then f
  else Float.pred f

let eio_of_statx x =
  let module X = Uring.Statx in
  { Eio.File.Stat.
    dev     = X.dev x;
    ino     = X.ino x;
    kind    = X.kind x;
    perm    = X.perm x;
    nlink   = X.nlink x;
    uid     = X.uid x;
    gid     = X.gid x;
    rdev    = X.rdev x;
    size    = X.size x |> Optint.Int63.of_int64;
    blksize = X.blksize x;
    blocks  = X.blocks x;
    atime   = float_of_time (X.atime_sec x) (X.atime_nsec x);
    mtime   = float_of_time (X.mtime_sec x) (X.mtime_nsec x);
    ctime   = float_of_time (X.ctime_sec x) (X.ctime_nsec x);
  }

external eio_mkdirat : Unix.file_descr -> string -> Unix.file_perm -> unit = "caml_eio_mkdirat"

external eio_renameat : Unix.file_descr -> string -> Unix.file_descr -> string -> unit = "caml_eio_renameat"

external eio_symlinkat : string -> Unix.file_descr -> string -> unit = "caml_eio_symlinkat"

external eio_getrandom : Cstruct.buffer -> int -> int -> int = "caml_eio_getrandom"

external eio_getdents : Unix.file_descr -> (Eio.File.Stat.kind * string) list = "caml_eio_getdents"

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

let rec enqueue_fsync fd st action =
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.fsync st.uring fd (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_fsync fd st action) st.io_q

let fsync fd =
  Fd.use_exn "fsync" fd @@ fun fd ->
  let res = Sched.enter "fsync" (enqueue_fsync fd) in
  match Res.unit_result res with
  | Ok () -> ()
  | Error e -> raise @@ Err.v e "fsync" ""

let rec enqueue_fdatasync fd st action =
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.fdatasync st.uring fd (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_fdatasync fd st action) st.io_q

let fdatasync fd =
  Fd.use_exn "fdatasync" fd @@ fun fd ->
  let res = Sched.enter "fdatasync" (enqueue_fdatasync fd) in
  match Res.unit_result res with
  | Ok () -> ()
  | Error e -> raise @@ Err.v e "fdatasync" ""

let rec enqueue_ftruncate fd len st action =
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.ftruncate st.uring fd ~len (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_ftruncate fd len st action) st.io_q

let ftruncate fd len =
  let len = Optint.Int63.to_int64 len in
  match Sched.op_supported Uring.Op.ftruncate with
  | true ->
    Fd.use_exn "ftruncate" fd @@ fun fd ->
    let res = Sched.enter "ftruncate" (enqueue_ftruncate fd len) in
    (match Res.unit_result res with
     | Ok () -> ()
     | Error e -> raise @@ Err.v e "ftruncate" "")
  | false -> begin
    try
      Eio_unix.run_in_systhread ~label:"ftruncate" @@ fun () ->
      Fd.use_exn "ftruncate" fd @@ fun fd ->
      Unix.LargeFile.ftruncate fd len
    with Unix.Unix_error (code, name, arg) -> raise @@ Err.v code name arg
  end

let rec enqueue_fallocate fd ~mode ~off ~len st action =
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.fallocate st.uring ~mode fd ~off ~len (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_fallocate fd ~mode ~off ~len st action) st.io_q

let fallocate ?(mode=Uring.Fallocate_flags.empty) fd ~off ~len =
  let off = Optint.Int63.to_int64 off in
  let len = Optint.Int63.to_int64 len in
  Fd.use_exn "fallocate" fd @@ fun fd ->
  let res = Sched.enter "fallocate" (enqueue_fallocate fd ~mode ~off ~len) in
  match Res.unit_result res with
  | Ok () -> ()
  | Error e -> raise @@ Err.v e "fallocate" ""

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
  match Res.unit_result res with
  | Error e -> raise @@ Err.v e "statx" path
  | Ok () -> ()

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

let fstat fd =
  let module X = Uring.Statx in
  let x = X.create () in
  statx_raw ~fd ~mask:X.Mask.basic_stats "" x X.Flags.empty_path;
  eio_of_statx x

let mkdir ~perm dir path =
  (* [mkdir] is really an operation on [path]'s parent. Get a reference to that first: *)
  with_parent_dir "mkdir" dir path @@ fun parent leaf ->
  try eio_mkdirat parent leaf perm
  with Unix.Unix_error (code, name, arg) -> raise @@ Err.v code name arg

let unlink ~rmdir dir path =
  (* [unlink] is really an operation on [path]'s parent. Get a reference to that first: *)
  with_parent_dir "unlink" dir path @@ fun parent leaf ->
  let res = Sched.enter "unlink" (enqueue_unlink (rmdir, parent, leaf)) in
  match Res.unit_result res with
  | Error e -> raise @@ Err.v e "unlinkat" ""
  | Ok () -> ()

let rename old_dir old_path new_dir new_path =
  with_parent_dir "renameat-old" old_dir old_path @@ fun old_parent old_leaf ->
  with_parent_dir "renameat-new" new_dir new_path @@ fun new_parent new_leaf ->
  try
    eio_renameat
      old_parent old_leaf
      new_parent new_leaf
  with Unix.Unix_error (code, name, arg) -> raise @@ Err.v code name arg

let symlink ~link_to dir path =
  with_parent_dir "symlinkat-new" dir path @@ fun parent leaf ->
  try
    eio_symlinkat link_to parent leaf
  with Unix.Unix_error (code, name, arg) -> raise @@ Err.v code name arg

let rec enqueue_shutdown fd command st action =
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.shutdown st.uring fd command (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_shutdown fd command st action) st.io_q

let shutdown socket command =
  Fd.use_exn "shutdown" socket @@ fun fd ->
  let res = Sched.enter "shutdown" (enqueue_shutdown fd command) in
  match Res.unit_result res with
  | Ok () -> ()
  | Error Unix.ENOTCONN -> ()
  | Error e -> raise @@ Err.v e "shutdown" ""

let rec enqueue_socket domain ty protocol st action =
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      (* The default [flags] sets close-on-exec on the new socket. *)
      Uring.socket st.uring domain ty protocol (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_socket domain ty protocol st action) st.io_q

let socket ~sw domain ty protocol =
  let fd =
    if Sched.op_supported Uring.Op.socket then (
      let res = Sched.enter "socket" (enqueue_socket domain ty protocol) in
      match Uring.Res.fd_result res with
      | Ok fd -> fd
      | Error e -> raise @@ Err.v e "socket" ""
    ) else (
      (* IORING_OP_SOCKET requires Linux >= 5.19; fall back to a blocking call. *)
      try Unix.socket ~cloexec:true domain ty protocol
      with Unix.Unix_error (code, name, arg) -> raise @@ Err.v code name arg
    )
  in
  Fd.of_unix ~sw ~seekable:false ~close_unix:true fd

let rec enqueue_bind fd addr st action =
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.bind st.uring fd addr (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_bind fd addr st action) st.io_q

let bind fd addr =
  Fd.use_exn "bind" fd @@ fun fd ->
  if Sched.op_supported Uring.Op.bind then (
    let res = Sched.enter "bind" (enqueue_bind fd addr) in
    match Res.unit_result res with
    | Ok () -> ()
    | Error e -> raise @@ Err.v e "bind" ""
  ) else (
    (* IORING_OP_BIND requires Linux >= 6.11; fall back to a blocking call. *)
    try Unix.bind fd addr
    with Unix.Unix_error (code, name, arg) -> raise @@ Err.v code name arg
  )

let rec enqueue_listen fd backlog st action =
  let retry = Sched.with_cancel_hook ~action st (fun () ->
      Uring.listen st.uring fd backlog (Job action)
    )
  in
  if retry then (* wait until an sqe is available *)
    Queue.push (fun st -> enqueue_listen fd backlog st action) st.io_q

let listen fd backlog =
  Fd.use_exn "listen" fd @@ fun fd ->
  if Sched.op_supported Uring.Op.listen then (
    let res = Sched.enter "listen" (enqueue_listen fd backlog) in
    match Res.unit_result res with
    | Ok () -> ()
    | Error e -> raise @@ Err.v e "listen" ""
  ) else (
    (* IORING_OP_LISTEN requires Linux >= 6.11; fall back to a blocking call. *)
    try Unix.listen fd backlog
    with Unix.Unix_error (code, name, arg) -> raise @@ Err.v code name arg
  )

let accept ~sw fd =
  Fd.use_exn "accept" fd @@ fun fd ->
  let client_addr = Uring.Sockaddr.create () in
  let res = Sched.enter "accept" (enqueue_accept fd client_addr) in
  match Uring.Res.fd_result res with
  | Error e -> raise @@ Err.v e "accept" ""
  | Ok unix ->
    let client = Fd.of_unix ~sw ~seekable:false ~close_unix:true unix in
    let client_addr = Uring.Sockaddr.get client_addr in
    client, client_addr

let read_dir fd =
  Fd.use_exn "read_dir" fd @@ fun fd ->
  let rec read_all acc fd =
    match eio_getdents fd with
    | [] -> acc
    | files ->
      let files = List.map snd files in
      read_all (files @ acc) fd
  in
  Eio_unix.run_in_systhread ~label:"read_dir" (fun () -> read_all [] fd)

let read_some_dir fd =
  Fd.use_exn "read_some_dir" fd @@ fun fd ->
  Eio_unix.run_in_systhread ~label:"read_some_dir" @@ fun () ->
  eio_getdents fd

let read_link fd path =
  try
    with_parent_dir_fd fd path @@ fun parent leaf ->
    Eio_unix.run_in_systhread ~label:"read_link" (fun () -> Eio_unix.Private.read_link (Some parent) leaf)
  with Unix.Unix_error (code, name, arg) -> raise @@ Err.v code name arg

let chmod ~follow ~mode dirfd path =
  try
    Switch.run @@ fun sw ->
    let fd =
      openat ~sw ~access:`R ~perm:0 dirfd path
        ~flags:Uring.Open_flags.(cloexec + path + if follow then empty else nofollow)
    in
    Eio_unix.run_in_systhread ~label:"chmod" (fun () ->
        try
          Eio_unix.Private.chmod ~mode fd ""
            ~flags:(Uring.Statx.Flags.empty_path :> int)
        with
        | Unix.Unix_error ((ENOSYS | EOPNOTSUPP | EINVAL) as orig_code, orig_fn, orig_arg) ->
          (* For Linux before 6.6 *)
          try
            Eio_unix.Fd.use_exn "chmod" fd @@ fun fd ->
            let fd : int = Obj.magic fd in
            Unix.chmod (Printf.sprintf "/proc/self/fd/%d" fd) mode
          with Unix.Unix_error _ ->
            (* The original error is less confusing *)
            let unix_error = Eio_unix.Unix_error (orig_code, orig_fn, orig_arg) in
            raise @@ Eio.Exn.create (Eio.Exn.Not_available unix_error)
      )
  with Unix.Unix_error (code, name, arg) -> raise @@ Err.v code name arg

let chown ~follow ?(uid=(-1L)) ?(gid=(-1L)) dirfd path =
  try
    Switch.run @@ fun sw ->
    let flags = Uring.Open_flags.(cloexec + path + if follow then empty else nofollow) in
    let fd = openat ~sw ~flags ~access:`R ~perm:0 dirfd path in
    Eio_unix.run_in_systhread ~label:"chown" (fun () ->
        let flags = (Uring.Statx.Flags.empty_path :> int) in
        Eio_unix.Private.chown ~flags ~uid ~gid fd ""
      )
  with Unix.Unix_error (code, name, arg) -> raise @@ Err.v code name arg

let getaddrinfo ~service node =
  Eio_unix.run_in_systhread ~label:"getaddrinfo" @@ fun () ->
  Err.run (Eio_unix.Private.getaddrinfo ~service) node

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
    | err -> Fork_action.report_spawn_error err
end

let setsockopt = Eio_unix.Private.setsockopt
let getsockopt = Eio_unix.Private.getsockopt
