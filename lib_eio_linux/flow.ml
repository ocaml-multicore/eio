open Eio.Std

(* When copying between a source with an FD and a sink with an FD, we can share the chunk
   and avoid copying. *)
let fast_copy src dst =
  let fallback () =
    (* No chunks available. Use regular memory instead. *)
    let buf = Cstruct.create 4096 in
    try
      while true do
        let got = Low_level.readv src [buf] in
        Low_level.writev dst [Cstruct.sub buf 0 got]
      done
    with End_of_file -> ()
  in
  Low_level.with_chunk ~fallback @@ fun chunk ->
  let chunk_size = Uring.Region.length chunk in
  try
    while true do
      let got = Low_level.read_upto src chunk chunk_size in
      Low_level.write dst chunk got
    done
  with End_of_file -> ()

(* Try a fast copy using splice. If the FDs don't support that, switch to copying. *)
let _fast_copy_try_splice src dst =
  try
    while true do
      let _ : int = Low_level.splice src ~dst ~len:max_int in
      ()
    done
  with
  | End_of_file -> ()
  | Eio.Exn.Io (Eio.Exn.X Eio_unix.Unix_error ((EAGAIN | EINVAL), "splice", _), _) -> fast_copy src dst

(* XXX workaround for issue #319, PR #327 *)
let fast_copy_try_splice src dst = fast_copy src dst
    
let[@tail_mod_cons] rec list_take n = function
  | [] -> []
  | x :: xs ->
    if n = 0 then []
    else x :: list_take (n - 1) xs

let truncate_to_iomax xs =
  if List.compare_length_with xs Uring.iov_max <= 0 then xs
  else list_take Uring.iov_max xs

(* Copy using the [Read_source_buffer] optimisation.
   Avoids a copy if the source already has the data. *)
let copy_with_rsb rsb dst =
  let write xs = Low_level.writev_single dst (truncate_to_iomax xs) in
  try
    while true do rsb write done
  with End_of_file -> ()

(* Copy by allocating a chunk from the pre-shared buffer and asking
   the source to write into it. This used when the other methods
   aren't available. *)
let fallback_copy (type src) (module Src : Eio.Flow.Pi.SOURCE with type t = src) src dst =
  let fallback () =
    (* No chunks available. Use regular memory instead. *)
    let buf = Cstruct.create 4096 in
    try
      while true do
        let got = Src.single_read src buf in
        Low_level.writev dst [Cstruct.sub buf 0 got]
      done
    with End_of_file -> ()
  in
  Low_level.with_chunk ~fallback @@ fun chunk ->
  let chunk_cs = Uring.Region.to_cstruct chunk in
  try
    while true do
      let got = Src.single_read src chunk_cs in
      Low_level.write dst chunk got
    done
  with End_of_file -> ()

module Impl = struct
  type tag = [`Generic | `Unix]

  type t = Eio_unix.Fd.t

  let fd t = t

  let close = Eio_unix.Fd.close

  let stat = Low_level.fstat

  let single_read t buf =
    Low_level.readv t [buf]

  let pread t ~file_offset bufs =
    Low_level.readv ~file_offset t bufs

  let pwrite t ~file_offset bufs =
    Low_level.writev_single ~file_offset t (truncate_to_iomax bufs)

  let read_methods = []

  let single_write t bufs = Low_level.writev_single t (truncate_to_iomax bufs)

  let copy t ~src =
    match Eio_unix.Resource.fd_opt src with
    | Some src -> fast_copy_try_splice src t
    | None ->
      let Eio.Resource.T (src, ops) = src in
      let module Src = (val (Eio.Resource.get ops Eio.Flow.Pi.Source)) in
      let rec aux = function
        | Eio.Flow.Read_source_buffer rsb :: _ -> copy_with_rsb (rsb src) t
        | _ :: xs -> aux xs
        | [] -> fallback_copy (module Src) src t
      in
      aux Src.read_methods

  let shutdown t cmd =
    Low_level.shutdown t @@ match cmd with
    | `Receive -> Unix.SHUTDOWN_RECEIVE
    | `Send -> Unix.SHUTDOWN_SEND
    | `All -> Unix.SHUTDOWN_ALL

  let send_msg t ~fds data =
    Low_level.send_msg t ~fds data

  let recv_msg_with_fds t ~sw ~max_fds data =
    let _addr, n, fds = Low_level.recv_msg_with_fds t ~sw ~max_fds data in
    n, fds

  let seek = Low_level.lseek
  let sync = Low_level.fsync
  let truncate = Low_level.ftruncate
end

let flow_handler = Eio_unix.Pi.flow_handler (module Impl)

let of_fd fd =
  let r = Eio.Resource.T (fd, flow_handler) in
  (r : [`Unix_fd | Eio_unix.Net.stream_socket_ty | Eio.File.rw_ty] r :>
     [< `Unix_fd | Eio_unix.Net.stream_socket_ty | Eio.File.rw_ty] r)

let source fd = (of_fd fd :> Eio_unix.source_ty r)
let sink   fd = (of_fd fd :> Eio_unix.sink_ty r)

let stdin = source Eio_unix.Fd.stdin
let stdout = sink Eio_unix.Fd.stdout
let stderr = sink Eio_unix.Fd.stderr

module Secure_random = struct
  type t = unit
  let single_read () buf = Low_level.getrandom buf; Cstruct.length buf
  let read_methods = []
end

let secure_random =
  let ops = Eio.Flow.Pi.source (module Secure_random) in
  Eio.Resource.T ((), ops)
