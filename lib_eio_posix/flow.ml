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
        atime   = ust.st_atime;
        mtime   = ust.st_mtime;
        ctime   = ust.st_ctime;
      }
    with Unix.Unix_error (code, name, arg) -> raise @@ Err.wrap code name arg

  let write t bufs =
    try
      let rec loop = function
        | [] -> ()
        | bufs ->
          let wrote = Low_level.writev t (Array.of_list bufs) in
          loop (Cstruct.shiftv bufs wrote)
      in
      loop bufs
    with Unix.Unix_error (code, name, arg) -> raise (Err.wrap code name arg)

  let copy dst ~src =
    let buf = Cstruct.create 4096 in
    try
      while true do
        let got = Eio.Flow.single_read src buf in
        write dst [Cstruct.sub buf 0 got]
      done
    with End_of_file -> ()

  let single_read t buf =
    match Low_level.readv t [| buf |] with
    | 0 -> raise End_of_file
    | got -> got
    | exception (Unix.Unix_error (code, name, arg)) -> raise (Err.wrap code name arg)

  let shutdown t cmd =
    try
      Low_level.shutdown t @@ match cmd with
      | `Receive -> Unix.SHUTDOWN_RECEIVE
      | `Send -> Unix.SHUTDOWN_SEND
      | `All -> Unix.SHUTDOWN_ALL
    with
    | Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()
    | Unix.Unix_error (code, name, arg) -> raise (Err.wrap code name arg)

  let read_methods = []

  let pread t ~file_offset bufs =
    let got = Low_level.preadv ~file_offset t (Array.of_list bufs) in
    if got = 0 then raise End_of_file
    else got

  let pwrite t ~file_offset bufs = Low_level.pwritev ~file_offset t (Array.of_list bufs)

  let send_msg t ~fds data =
    Low_level.send_msg ~fds t (Array.of_list data)

  let recv_msg_with_fds t ~sw ~max_fds data =
    let _addr, n, fds = Low_level.recv_msg_with_fds t ~sw ~max_fds (Array.of_list data) in
    n, fds

  let fd t = t

  let close = Eio_unix.Fd.close
end

let handler = Eio_unix.Resource.flow_handler (module Impl)

let of_fd fd =
  let r = Eio.Resource.T (fd, handler) in
  (r : [`Unix_fd | Eio_unix.Net.stream_socket_ty | Eio.File.rw_ty] r :>
     [< `Unix_fd | Eio_unix.Net.stream_socket_ty | Eio.File.rw_ty] r)

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
