open Eio.Std

module Fd = Eio_unix.Fd

module Impl = struct
  type tag = [`Generic | `Unix]

  type t = Eio_unix.Fd.t

  let stat t k =
    try
      let ust = Low_level.fstat t in
      let st_kind : Eio.File.kind =
        match ust.st_kind with
        | Unix.S_REG  -> `Regular_file
        | Unix.S_DIR  -> `Directory
        | Unix.S_CHR  -> `Character_special
        | Unix.S_BLK  -> `Block_device
        | Unix.S_LNK  -> `Symbolic_link
        | Unix.S_FIFO -> `Fifo
        | Unix.S_SOCK -> `Socket
      in
      let rec fn : type a b. (a, b) Eio.File.stats -> a -> b = fun v acc ->
        match v with
        | Dev :: tl -> fn tl @@ acc (ust.st_dev |> Int64.of_int)
        | Ino :: tl -> fn tl @@ acc (ust.st_ino |> Int64.of_int)
        | Kind :: tl -> fn tl @@ acc st_kind
        | Perm :: tl -> fn tl @@ acc ust.st_perm
        | Nlink :: tl -> fn tl @@ acc (ust.st_nlink |> Int64.of_int)
        | Uid :: tl -> fn tl @@ acc (ust.st_uid |> Int64.of_int)
        | Gid :: tl -> fn tl @@ acc (ust.st_gid |> Int64.of_int)
        | Rdev :: tl -> fn tl @@ acc (ust.st_rdev |> Int64.of_int)
        | Size :: tl -> fn tl @@ acc ust.st_size
        | Atime :: tl -> fn tl @@ acc ust.st_atime
        | Mtime :: tl -> fn tl @@ acc ust.st_mtime
        | Ctime :: tl -> fn tl @@ acc ust.st_ctime
        | [] -> acc
      in
      fn k
    with Unix.Unix_error (code, name, arg) -> raise @@ Err.wrap code name arg

  let write_all t bufs =
    try Low_level.writev t bufs
    with Unix.Unix_error (code, name, arg) -> raise (Err.wrap code name arg)

  (* todo: provide a way to do a single write *)
  let single_write t bufs =
    write_all t bufs;
    Cstruct.lenv bufs

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

  let single_read t buf =
    match Low_level.read_cstruct t buf with
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

  let send_msg _t ~fds:_ _data = failwith "Not implemented on Windows"

  let recv_msg_with_fds _t ~sw:_ ~max_fds:_ _data = failwith "Not implemented on Windows"

  let fd t = t

  let close = Eio_unix.Fd.close
end

let handler = Eio_unix.Pi.flow_handler (module Impl)

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
