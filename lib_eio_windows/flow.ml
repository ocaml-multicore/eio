open Eio.Std

module Fd = Eio_unix.Fd

module Impl = struct
  type tag = [`Generic | `Unix]

  type t = { fd : Fd.t; mutable kind : File_io.kind option }

  let kind t raw =
    match t.kind with
    | Some k -> k
    | None ->
      let k = File_io.kind raw in
      t.kind <- Some k;
      k

  let stat_fd t =
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

  let stat t = stat_fd t.fd

  let rec single_write t = function
    | [] -> 0
    | buf :: (_ :: _ as rest) when Cstruct.is_empty buf -> single_write t rest
    | buf :: _ ->
      match Err.run (File_io.write_cstruct (kind t) t.fd) buf with
      | 0 when not (Cstruct.is_empty buf) -> raise (Err.v EIO "write" "")
      | n -> n

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

  let single_read t buf =
    match Err.run (File_io.read_cstruct (kind t) t.fd) buf with
    | 0 -> raise End_of_file
    | got -> got

  let shutdown t cmd =
    Err.run (Low_level.shutdown t.fd) @@ match cmd with
    | `Receive -> Unix.SHUTDOWN_RECEIVE
    | `Send -> Unix.SHUTDOWN_SEND
    | `All -> Unix.SHUTDOWN_ALL

  let read_methods = []

  let pread t ~file_offset bufs =
    let got = Err.run (File_io.positioned ~write:false ~file_offset t.fd List.find_opt) bufs in
    if got = 0 then raise End_of_file
    else got

  let pwrite t ~file_offset bufs = Err.run (File_io.positioned ~write:true ~file_offset t.fd List.find_opt) bufs

  let send_msg _t ~fds:_ _data = failwith "Not implemented on Windows"

  let recv_msg_with_fds _t ~sw:_ ~max_fds:_ _data = failwith "Not implemented on Windows"

  let seek t = Low_level.lseek t.fd
  let sync t = Low_level.fsync t.fd
  let truncate t = Low_level.ftruncate t.fd

  let fd t = t.fd

  let close t = Fd.close t.fd

  let setsockopt t opt v = Eio_unix.Private.setsockopt t.fd opt v
  let getsockopt t opt = Eio_unix.Private.getsockopt t.fd opt
end

type (_, _, _) Eio.Resource.pi += Windows_flow : ('t, 't -> Impl.t, [> `Windows_flow]) Eio.Resource.pi

let get (Eio.Resource.T (v, ops)) =
  Option.map (fun get -> get v) (Eio.Resource.get_opt ops Windows_flow)

let kind flow =
  match get flow with
  | Some t -> Impl.kind t
  | None -> File_io.kind

let with_flow ops =
  Eio.Resource.handler (H (Windows_flow, Fun.id) :: Eio.Resource.bindings ops)

let file_handler = with_flow (Eio_unix.Pi.file_handler (module Impl))

let of_fd ?kind fd =
  let r = Eio.Resource.T ({ Impl.fd; kind }, file_handler) in
  (r : [`Unix_fd | Eio.File.rw_ty] r :>
     [< `Unix_fd | Eio.File.rw_ty] r)

let shared_sink ~sw a b =
  if a == b then Some a
  else match get a, get b with
    | Some a, Some b ->
      Fd.use_exn "spawn" a.fd @@ fun x ->
      Fd.use_exn "spawn" b.fd @@ fun y ->
      if x != y then None
      else (
        (* Either wrapper can close the descriptor while the child is running. *)
        let fd = Fd.of_unix ~sw ~close_unix:true (Unix.dup ~cloexec:true x) in
        Some (of_fd ?kind:a.kind fd :> Eio.Flow.sink_ty r)
      )
    | _ -> None

let stream_handler = with_flow (Eio_unix.Pi.stream_handler (module Impl))

let stream fd =
  let r = Eio.Resource.T ({ Impl.fd; kind = None }, stream_handler) in
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
