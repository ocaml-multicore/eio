(* Copyright (C) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: ISC *)

(** Adapt file handles to overlapped I/O.

    Positioned operations preserve the file position and reopen synchronous handles by
    handle when necessary. *)

type kind = Synchronous | Socket | Pipe | File | Append_file | Synchronous_file
external kind_raw : Unix.file_descr -> kind = "caml_eio_windows_fd_kind"

(** Classify the handle. Standard descriptors always use synchronous I/O. *)
let kind fd =
  (* Inherited standard handles may belong to the parent's completion port. *)
  if fd == Unix.stdin || fd == Unix.stdout || fd == Unix.stderr then Synchronous
  else kind_raw fd

external seek : Unix.file_descr -> int64 -> int -> int64 = "caml_eio_windows_seek"
external reopen : Unix.file_descr -> bool -> Unix.file_descr = "caml_eio_windows_reopen"

(** Transfer data at [offset] without changing the descriptor's file position. *)
let transfer_at ~write fd buf ~offset =
  let run fd = Completion.await (fun port ~data ->
      (if write then Iocp.Event.write else Iocp.Event.read) port ~data fd buf ~offset)
  in
  match kind fd with
  | File | Append_file -> run fd
  | Synchronous | Synchronous_file | Socket | Pipe ->
    let fd = Eio_unix.run_in_systhread (fun () -> reopen fd write) in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> run fd)

let positioned ~write ~file_offset resource find_opt bufs =
  let offset = Optint.Int63.to_int64 file_offset in
  if offset < 0L then invalid_arg "Negative file offset";
  Eio_unix.Fd.use_exn "positioned_io" resource @@ fun fd ->
  match find_opt (fun buf -> not (Cstruct.is_empty buf)) bufs with
  | None -> 0
  | Some buf -> transfer_at ~write fd buf ~offset
