open Eio.Std

let in_worker_thread = Eio_unix.run_in_systhread

module Fd = Eio_unix.Fd

let await_readable fd = Fd.use_exn "await_readable" fd Eio_unix.await_readable
let await_writable fd = Fd.use_exn "await_writable" fd Eio_unix.await_writable

type ty = Read | Write

let rec do_nonblocking ty fn fd =
  Fiber.yield ();
  try fn fd with
  | Unix.Unix_error (EINTR, _, _) -> 
    do_nonblocking ty fn fd    (* Just in case *)
  | Unix.Unix_error((EAGAIN | EWOULDBLOCK), _, _) ->
    (match ty with Read -> Eio_unix.await_readable fd | Write -> Eio_unix.await_writable fd);
    do_nonblocking ty fn fd

let read_cstruct fd (buf : Cstruct.t) =
  Fd.use_exn "read" fd @@ fun raw ->
  match File_io.kind raw with
  | File_io.Socket -> Completion.await (fun port ~data -> Iocp.Event.recv port ~data raw buf)
  | File_io.Pipe -> Completion.await (fun port ~data -> Iocp.Event.read port ~data raw buf ~offset:0L)
  | File_io.File | File_io.Append_file -> File_io.sequential ~write:false ~append:false raw buf
  | (File_io.Synchronous | File_io.Synchronous_file) as kind ->
    let read () = in_worker_thread (fun () -> Unix.read_bigarray raw buf.buffer buf.off buf.len) in
    File_io.Serial.with_fd raw (fun () ->
        if kind = File_io.Synchronous then Eio_unix.await_readable raw;
        read ())

let write_cstruct fd (buf : Cstruct.t) =
  Fd.use_exn "write" fd @@ fun raw ->
  match File_io.kind raw with
  | File_io.Socket -> Completion.await (fun port ~data -> Iocp.Event.send port ~data raw buf)
  | File_io.Pipe -> Completion.await (fun port ~data -> Iocp.Event.write port ~data raw buf ~offset:0L)
  | (File_io.File | File_io.Append_file) as kind ->
    File_io.sequential ~write:true ~append:(kind = File_io.Append_file) raw buf
  | File_io.Synchronous | File_io.Synchronous_file -> in_worker_thread (fun () -> Unix.single_write_bigarray raw buf.buffer buf.off buf.len)

let read fd buf start len =
  if start < 0 || len < 0 || start > Bytes.length buf - len then invalid_arg "read";
  let tmp = Cstruct.create len in
  let n = read_cstruct fd tmp in
  Cstruct.blit_to_bytes tmp 0 buf start n;
  n

let write fd buf start len =
  if start < 0 || len < 0 || start > Bytes.length buf - len then invalid_arg "write";
  write_cstruct fd (Cstruct.of_bytes ~off:start ~len buf)

let sleep_until = Sched.sleep_until

let socket ~sw socket_domain socket_type protocol =
  Switch.check sw;
  let sock_unix = Unix.socket ~cloexec:true socket_domain socket_type protocol in
  match Unix.set_nonblock sock_unix with
  | () -> Fd.of_unix ~sw ~blocking:false ~close_unix:true sock_unix
  | exception ex -> Unix.close sock_unix; raise ex

let connect fd addr =
  try
    Fd.use_exn "connect" fd (fun fd -> Unix.connect fd addr)
  with
  | Unix.Unix_error ((EINTR | EAGAIN | EWOULDBLOCK | EINPROGRESS), _, _) ->
    await_writable fd;
    match Fd.use_exn "connect" fd Unix.getsockopt_error with
    | None -> ()
    | Some code -> raise (Err.v code "connect-in-progress" "")

let accept ~sw sock =
  Fd.use_exn "accept" sock @@ fun sock ->
  let client, addr =
    do_nonblocking Read (fun fd -> Switch.check sw; Unix.accept ~cloexec:true fd) sock
  in
  Unix.set_nonblock client;
  Fd.of_unix ~sw ~blocking:false ~close_unix:true client, addr

let shutdown sock cmd =
  Fd.use_exn "shutdown" sock (fun fd -> Unix.shutdown fd cmd)

let send_msg fd ?dst buf =
  Fd.use_exn "send_msg" fd @@ fun fd ->
  do_nonblocking Write (fun fd ->
      match dst with
      | Some dst -> Unix.sendto fd buf 0 (Bytes.length buf) [] dst
      | None -> Unix.send fd buf 0 (Bytes.length buf) []
    ) fd

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

external eio_reparse_point : Unix.file_descr -> (string * bool * int) option = "caml_eio_windows_reparse_point"

let reparse_target fd =
  Fd.use_exn "read_link" fd @@ fun fd ->
  Option.map (fun (target, relative, _) ->
      if relative then Eio_utils.Nt_path.Relative target else Absolute target
    ) (eio_reparse_point fd)

let fstat fd =
  Fd.use_exn "fstat" fd @@ fun fd ->
  let st = Unix.LargeFile.fstat fd in
  match eio_reparse_point fd with
  | Some (_, _, size) -> { st with st_kind = S_LNK; st_size = Int64.of_int size }
  | None -> st

let lstat path =
  in_worker_thread @@ fun () ->
  Unix.LargeFile.lstat path

external full_path : string -> string = "caml_eio_windows_full_path"

let realpath path =
  in_worker_thread @@ fun () ->
  Unix.realpath path

external eio_realpath_fd : Unix.file_descr -> string = "caml_eio_windows_realpath_fd"

let realpath_fd fd =
  Fd.use_exn "realpath" fd @@ fun fd ->
  in_worker_thread (fun () -> eio_realpath_fd fd)

external eio_readdir_batch : Unix.file_descr -> bool -> string list = "caml_eio_windows_readdir_batch"

let readdir_fd fd =
  Fd.use_exn "readdir" fd @@ fun fd ->
  in_worker_thread @@ fun () ->
  let rec loop restart acc =
    match eio_readdir_batch fd restart with
    | [] -> Array.of_list (List.filter (fun name -> name <> "." && name <> "..") acc)
    | names -> loop false (List.rev_append names acc)
  in
  loop true []

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

let chown ?dirfd ~follow:_ ?(uid=(-1L)) ?(gid=(-1L)) path =
  in_worker_thread @@ fun () ->
  match dirfd with
  | None -> failwith "Chown is unsupported on Windows"
  | Some dirfd -> Eio_unix.Private.chown ~flags:0 ~uid ~gid dirfd path

let readv fd bufs =
  match Array.find_opt (fun b -> Cstruct.length b > 0) bufs with
  | None -> 0
  | Some buf -> read_cstruct fd buf

let writev fd bufs =
  let rec loop buf = if Cstruct.length buf > 0 then (
    let n = write_cstruct fd buf in
    if n = 0 then raise (Unix.Unix_error (EIO, "write", ""));
    loop (Cstruct.shift buf n)
  ) in
  List.iter loop bufs

let preadv ~file_offset fd bufs = File_io.positioned ~write:false ~file_offset fd Array.find_opt bufs
let pwritev ~file_offset fd bufs = File_io.positioned ~write:true ~file_offset fd Array.find_opt bufs

module Flags = struct
  module Open = struct
    type t = int
    let rdonly = Config.o_rdonly
    let rdwr = Config.o_rdwr
    let wronly = Config.o_wronly
    let cloexec = Config.o_noinherit
    let creat = Config.o_creat
    let excl = Config.o_excl
    let trunc = Config.o_trunc

    let generic_read = Config.generic_read
    let generic_write = Config.generic_write
    let synchronise = Config.synchronize
    let append = Config.file_append_data

    let empty = 0
    let ( + ) = ( lor )
  end

  module Disposition = struct
    type t = int
    let supersede = Config.file_supersede
    let create = Config.file_create
    let open_ = Config.file_open
    let open_if = Config.file_open_if
    let overwrite = Config.file_overwrite
    let overwrite_if = Config.file_overwrite_if
  end

  module Create = struct
    type t = int
    let empty = 0
    let directory = Config.file_directory_file
    let non_directory = Config.file_non_directory_file
    let no_intermediate_buffering = Config.file_no_intermediate_buffering
    let write_through = Config.file_write_through
    let sequential_only = Config.file_sequential_only
    let ( + ) = ( lor )
  end
end

let rec with_dirfd op dirfd fn =
  match dirfd with
  | None -> fn None
  | Some dirfd -> Fd.use_exn op dirfd (fun fd -> fn (Some fd))
  | exception Unix.Unix_error(Unix.EINTR, _, "") -> with_dirfd op dirfd fn

let nt_path dirfd path =
  match dirfd with
  | Some _ -> path
  | None ->
    let module P = Eio_utils.Nt_path in
    let path =
      if P.classify path = `Verbatim then path
      else full_path (if path = "" then "." else path)
    in
    P.to_nt ~cwd:"" path

type follow = Follow | Nofollow | Open_link

external eio_openat :
  Unix.file_descr option -> beneath:bool -> open_link:bool -> string ->
  Flags.Open.t -> Flags.Disposition.t -> Flags.Create.t -> Unix.file_descr
  = "caml_eio_windows_openat_bytes" "caml_eio_windows_openat"

let openat ?dirfd ?(beneath=false) ?(follow=Follow) ~sw path flags dis create =
  with_dirfd "openat" dirfd @@ fun dirfd ->
  Switch.check sw;
  let path = nt_path dirfd path in
  let flags = Flags.Open.(flags + cloexec) in
  (* FILE_CREATE must test the leaf itself, even if it is a dangling link. *)
  let open_link = follow = Open_link || dis = Flags.Disposition.create in
  in_worker_thread ~label:"openat" (fun () ->
      match Eio_utils.Nt_path.split path with
      | Some (parent, leaf) when follow = Nofollow && not beneath ->
        (* Refuse a leaf link before any side effect, including truncation. *)
        let parent =
          eio_openat dirfd ~beneath:false ~open_link:false parent
            Flags.Open.synchronise Flags.Disposition.open_ Flags.Create.directory
        in
        Fun.protect ~finally:(fun () -> Unix.close parent) @@ fun () ->
        eio_openat (Some parent) ~beneath:true ~open_link leaf flags dis create
      | _ -> eio_openat dirfd ~beneath ~open_link path flags dis create
    )
  |> Fd.of_unix ~sw ~blocking:true ~close_unix:true

let read_link ?dirfd path =
  Switch.run @@ fun sw ->
  let fd = openat ?dirfd ~follow:Open_link ~sw path Flags.Open.synchronise Flags.Disposition.open_ Flags.Create.empty in
  match reparse_target fd with
  | Some (Eio_utils.Nt_path.Relative target) -> target
  | Some (Eio_utils.Nt_path.Absolute target) -> Eio_utils.Nt_path.to_win32 target
  | None -> raise (Unix.Unix_error (EINVAL, "readlink", path))

let mkdir ?dirfd ?(follow=Follow) ~mode:_ path =
  Switch.run @@ fun sw ->
  let _ : Fd.t = openat ?dirfd ~follow ~sw path Flags.Open.(generic_write + synchronise) Flags.Disposition.(create) Flags.Create.(directory) in
  ()

external eio_unlinkat : Unix.file_descr option -> string -> bool -> unit = "caml_eio_windows_unlinkat"

let unlink ?dirfd ~dir path =
  with_dirfd "unlink" dirfd @@ fun dirfd ->
  let path = nt_path dirfd path in
  in_worker_thread ~label:"unlink" @@ fun () ->
  eio_unlinkat dirfd path dir

external eio_renameat : Unix.file_descr option -> string -> Unix.file_descr option -> string -> unit = "caml_eio_windows_renameat"

let rename ?old_dir old_path ?new_dir new_path =
  with_dirfd "rename-old" old_dir @@ fun old_dir ->
  with_dirfd "rename-new" new_dir @@ fun new_dir ->
  let old_path = nt_path old_dir old_path and new_path = nt_path new_dir new_path in
  in_worker_thread ~label:"rename" @@ fun () ->
  eio_renameat old_dir old_path new_dir new_path


external eio_symlinkat : Unix.file_descr -> string -> bool -> (string * string * bool) -> unit = "caml_eio_windows_symlinkat"
let symlink_target link_to =
  let module P = Eio_utils.Nt_path in
  let link_to = if P.classify link_to = `Drive_relative then full_path link_to else link_to in
  P.link_target ~cwd:(Sys.getcwd ()) link_to

let symlink_raw ~to_dir ~target new_dir new_path =
  let module P = Eio_utils.Nt_path in
  let target, relative = match target with P.Relative p -> p, true | P.Absolute p -> p, false in
  match new_dir with
  | None -> in_worker_thread ~label:"symlink" (fun () -> Unix.symlink ~to_dir (P.to_win32 target) new_path)
  | Some dir ->
    let data = target, P.to_win32 target, relative in
    Fd.use_exn "symlink" dir @@ fun dir ->
    in_worker_thread ~label:"symlink" (fun () -> eio_symlinkat dir new_path to_dir data)

let symlink ?(to_dir=false) ~link_to new_dir new_path =
  symlink_raw ~to_dir ~target:(symlink_target link_to) new_dir new_path

let chmod ~mode new_dir new_path =
  with_dirfd "chmod" new_dir @@ fun new_dir ->
  match new_dir with
  | Some _ -> raise (Unix.Unix_error (Unix.EOPNOTSUPP, "chmod", new_path))
  | None ->
    in_worker_thread ~label:"chmod" @@ fun () ->
    Unix.chmod new_path mode

let lseek fd off cmd =
  Fd.use_exn "lseek" fd @@ fun raw ->
  File_io.seek raw (Optint.Int63.to_int64 off) (match cmd with `Set -> 0 | `Cur -> 1 | `End -> 2)
  |> Optint.Int63.of_int64

let fsync fd =
  Eio_unix.run_in_systhread @@ fun () ->
  Fd.use_exn "fsync" fd Unix.fsync

let ftruncate fd len =
  Eio_unix.run_in_systhread @@ fun () ->
  Fd.use_exn "ftruncate" fd @@ fun fd ->
  Unix.LargeFile.ftruncate fd (Optint.Int63.to_int64 len)

let pipe ~sw =
  Switch.check sw;
  let unix_r, unix_w = Iocp.pipe () in
  let r = Fd.of_unix ~sw ~blocking:false ~close_unix:true unix_r in
  let w = Fd.of_unix ~sw ~blocking:false ~close_unix:true unix_w in
  r, w
