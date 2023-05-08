module Fd = Eio_unix.Fd

let fstat fd =
  try
    let ust = Low_level.fstat fd in
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

let write_bufs fd bufs =
  try
    Low_level.writev fd bufs
  with Unix.Unix_error (code, name, arg) -> raise (Err.wrap code name arg)

let copy src dst =
  let buf = Cstruct.create 4096 in
  try
    while true do
      let got = Eio.Flow.single_read src buf in
      write_bufs dst [Cstruct.sub buf 0 got]
    done
  with End_of_file -> ()

let read fd buf =
  match Low_level.read_cstruct fd buf with
  | 0 -> raise End_of_file
  | got -> got
  | exception (Unix.Unix_error (code, name, arg)) -> raise (Err.wrap code name arg)

let shutdown fd cmd =
  try
    Low_level.shutdown fd @@ match cmd with
    | `Receive -> Unix.SHUTDOWN_RECEIVE
    | `Send -> Unix.SHUTDOWN_SEND
    | `All -> Unix.SHUTDOWN_ALL
  with Unix.Unix_error (code, name, arg) -> raise (Err.wrap code name arg)

let of_fd fd = object (_ : <Eio_unix.socket; Eio.File.rw>)
  method fd = fd

  method read_methods = []
  method copy src = copy src fd

  method pread ~file_offset bufs = Low_level.preadv ~file_offset fd (Array.of_list bufs)
  method pwrite ~file_offset bufs = Low_level.pwritev ~file_offset fd (Array.of_list bufs)

  method stat = fstat fd
  method read_into buf = read fd buf
  method write bufs = write_bufs fd bufs
  method shutdown cmd = shutdown fd cmd
  method close = Fd.close fd

  method probe : type a. a Eio.Generic.ty -> a option = function
    | Eio_unix.Resource.FD -> Some fd
    | _ -> None
end

let secure_random = object
  inherit Eio.Flow.source

  method read_into buf =
    Low_level.getrandom buf;
    Cstruct.length buf
end
