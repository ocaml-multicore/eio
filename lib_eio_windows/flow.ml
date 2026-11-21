open Eio.Std

module Fd = Eio_unix.Fd

module Impl = struct
  type tag = [`Generic | `Unix]

  type t = Eio_unix.Fd.t

  let stat t =
    try
      let ust = Low_level.fstat t in
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
        blksize = 0L;   (* Not available on Windows *)
        blocks  = 0L;   (* Not available on Windows *)
        atime   = ust.st_atime;
        mtime   = ust.st_mtime;
        ctime   = ust.st_ctime;
      }
    with Unix.Unix_error (code, name, arg) -> raise @@ Err.v code name arg

  let rec single_write t = function
    | [] -> 0
    | buf :: rest when Cstruct.is_empty buf -> single_write t rest
    | buf :: _ ->
      match Err.run (Low_level.write_cstruct t) buf with
      | 0 -> raise (Err.v EIO "write" "")
      | n -> n

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

  let single_read t buf =
    match Err.run (Low_level.read_cstruct t) buf with
    | 0 -> raise End_of_file
    | got -> got

  let shutdown t cmd =
    try
      Low_level.shutdown t @@ match cmd with
      | `Receive -> Unix.SHUTDOWN_RECEIVE
      | `Send -> Unix.SHUTDOWN_SEND
      | `All -> Unix.SHUTDOWN_ALL
    with
    | Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()
    | Unix.Unix_error (code, name, arg) -> raise (Err.v code name arg)

  let read_methods = []

  let pread t ~file_offset bufs =
    let got = Err.run (File_io.positioned ~write:false ~file_offset t List.find_opt) bufs in
    if got = 0 then raise End_of_file
    else got

  let pwrite t ~file_offset bufs = Err.run (File_io.positioned ~write:true ~file_offset t List.find_opt) bufs

  let send_msg _t ~fds:_ _data = failwith "Not implemented on Windows"

  let recv_msg_with_fds _t ~sw:_ ~max_fds:_ _data = failwith "Not implemented on Windows"

  let seek = Low_level.lseek
  let sync = Low_level.fsync
  let truncate = Low_level.ftruncate

  let fd t = t

  let close = Eio_unix.Fd.close

  let setsockopt t opt v = Eio_unix.Private.setsockopt t opt v
  let getsockopt t opt = Eio_unix.Private.getsockopt t opt
end

let file_handler = Eio_unix.Pi.file_handler (module Impl)

let of_fd fd =
  let r = Eio.Resource.T (fd, file_handler) in
  (r : [`Unix_fd | Eio.File.rw_ty] r :>
     [< `Unix_fd | Eio.File.rw_ty] r)

let stream_handler = Eio_unix.Pi.stream_handler (module Impl)

let stream fd =
  let r = Eio.Resource.T (fd, stream_handler) in
  (r : [`Unix_fd | Eio_unix.Net.stream_socket_ty] r :>
     [< `Unix_fd | Eio_unix.Net.stream_socket_ty] r)

module Secure_random = struct
  type t = unit

  let single_read () buf =
    Low_level.getrandom buf;
    Cstruct.length buf

  let read_methods = []
end

let secure_random =
  let ops = Eio.Flow.Pi.source (module Secure_random) in
  Eio.Resource.T ((), ops)
