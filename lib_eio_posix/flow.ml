open Eio.Std

module Fd = Eio_unix.Fd

let float_of_time s ns =
  let s = Int64.to_float s in
  let f = s +. (float ns /. 1e9) in
  (* It's possible that we might round up to the next second.
     Since some algorithms only care about the seconds part,
     make sure the integer part is always [s]: *)
  if floor f = s then f
  else Float.pred f

module Impl = struct
  type tag = [`Generic | `Unix]

  type t = Eio_unix.Fd.t

  let stat t k =
    let open Eio.File in
    let x = Low_level.create_stat () in
    Low_level.fstat ~buf:x t;
    try
      let rec fn : type a b. (a, b) stats -> a -> b = fun v acc ->
        match v with
        | Dev :: tl -> fn tl @@ acc (Low_level.dev x)
        | Ino :: tl -> fn tl @@ acc (Low_level.ino x)
        | Kind :: tl -> fn tl @@ acc (Low_level.kind x)
        | Perm :: tl -> fn tl @@ acc (Low_level.perm x)
        | Nlink :: tl -> fn tl @@ acc (Low_level.nlink x)
        | Uid :: tl -> fn tl @@ acc (Low_level.uid x)
        | Gid :: tl -> fn tl @@ acc (Low_level.gid x)
        | Rdev :: tl -> fn tl @@ acc (Low_level.rdev x)
        | Size :: tl -> fn tl @@ acc (Low_level.size x)
        | Atime :: tl -> fn tl @@ acc (float_of_time (Low_level.atime_sec x) (Low_level.atime_nsec x))
        | Mtime :: tl -> fn tl @@ acc (float_of_time (Low_level.mtime_sec x) (Low_level.mtime_nsec x))
        | Ctime :: tl -> fn tl @@ acc (float_of_time (Low_level.ctime_sec x) (Low_level.ctime_nsec x))
        | [] -> acc
      in
      fn k
    with Unix.Unix_error (code, name, arg) -> raise @@ Err.wrap code name arg

  let single_write t bufs =
    try
      Low_level.writev t (Array.of_list bufs)
    with Unix.Unix_error (code, name, arg) ->
      raise (Err.wrap code name arg)

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

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
