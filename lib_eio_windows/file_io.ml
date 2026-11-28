(* Copyright (C) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: ISC *)

(** Adapt file handles to overlapped I/O.

    Sequential operations serialize access to each descriptor's file position.
    Positioned operations leave it unchanged and reopen synchronous handles by
    handle when necessary. *)

type kind = Pollable | Socket | Pipe | File | Append_file | Synchronous | Sync_pipe
external kind_raw : Unix.file_descr -> kind = "caml_eio_windows_fd_kind"
external pipe_kind : Unix.file_descr -> kind = "caml_eio_windows_pipe_kind"

module Serial = struct
  let lock = Mutex.create ()
  let cursors = ref []
  let blocking_io = ref []

  (* Each entry borrows its descriptor through Fd.use until it leaves the list.
     Duplicates of the same handle share the mutex for each kind of operation. *)
  let with_fd ?(users = cursors) fd fn =
    let entry = Mutex.protect lock (fun () ->
        let mutex = match List.find_opt (fun (other, _) ->
            fd == other || Iocp.same_handle fd other) !users with
          | Some (_, mutex) -> mutex
          | None -> Eio.Mutex.create ()
        in
        let entry = fd, mutex in
        users := entry :: !users;
        entry)
    in
    Fun.protect ~finally:(fun () ->
        Mutex.protect lock (fun () -> users := List.filter ((!=) entry) !users))
      (fun () -> Eio.Mutex.use_ro (snd entry) fn)

  let with_blocking_io fd fn = with_fd ~users:blocking_io fd fn
end

let kind fd =
  match kind_raw fd with
  | Pipe ->
    (* A mode query waits behind synchronous pipe I/O in the kernel. Queue it
       here first so the wait can be cancelled. *)
    Serial.with_blocking_io fd (fun () -> Eio_unix.run_in_systhread (fun () -> pipe_kind fd))
  | kind -> kind

external seek_raw : Unix.file_descr -> int64 -> int -> int64 = "caml_eio_windows_seek"
type reopen_mode = Read | Write | Append
external reopen : Unix.file_descr -> reopen_mode -> Unix.file_descr = "caml_eio_windows_reopen"
(** Return the original descriptor if sharing prevents opening a second handle. *)

(** Change the file position while excluding sequential I/O on this descriptor. *)
let seek fd off whence =
  let move () = seek_raw fd off whence in
  match kind_raw fd with
  | File | Append_file | Synchronous -> Serial.with_fd fd move
  | Pollable -> move ()
  | Pipe | Sync_pipe | Socket -> raise (Unix.Unix_error (ESPIPE, "lseek", ""))

let append_shared fd (buf : Cstruct.t) =
  Eio_unix.run_in_systhread @@ fun () ->
  let out = reopen fd Append in
  if out == fd then None else (
    (* A synchronous append records its own end position, even if another handle appends next. *)
    Fun.protect ~finally:(fun () -> Unix.close out) @@ fun () ->
    let n = Unix.single_write_bigarray out buf.buffer buf.off buf.len in
    if n > 0 then ignore (seek_raw fd (seek_raw out 0L 1) 0);
    Some n
  )

(** Transfer data at the current position and advance it by the completed byte
    count, including when cancellation races with a successful transfer. *)
let sequential ~write ~append fd buf =
  Serial.with_fd fd @@ fun () ->
  match if append then append_shared fd buf else None with
  | Some n -> n
  | None ->
    let offset = if append then -1L else seek_raw fd 0L 1 in
    let on_complete (c : Iocp.completion) =
      if c.bytes_transferred > 0 then
        if append then ignore (seek_raw fd 0L 2)
        else ignore (seek_raw fd (Int64.add offset (Int64.of_int c.bytes_transferred)) 0)
    in
    Completion.await ~on_complete (fun port ~data ->
        (if write then Iocp.Event.write else Iocp.Event.read) port ~data fd buf ~offset)

(** Transfer data at [offset] without changing the descriptor's file position. *)
let transfer_at ~write fd buf ~offset =
  let run fd = Completion.await (fun port ~data ->
      (if write then Iocp.Event.write else Iocp.Event.read) port ~data fd buf ~offset)
  in
  match kind_raw fd with
  | File | Append_file -> run fd
  | Socket | Pipe | Sync_pipe ->
    raise (Unix.Unix_error (ESPIPE, (if write then "pwrite" else "pread"), ""))
  | Pollable | Synchronous ->
    let other = Eio_unix.run_in_systhread (fun () -> reopen fd (if write then Write else Read)) in
    if other == fd then raise (Unix.Unix_error (EOPNOTSUPP, "ReOpenFile", ""));
    Fun.protect ~finally:(fun () -> Unix.close other) (fun () -> run other)

let positioned ~write ~file_offset resource find_opt bufs =
  let offset = Optint.Int63.to_int64 file_offset in
  if offset < 0L then invalid_arg "Negative file offset";
  Eio_unix.Fd.use_exn "positioned_io" resource @@ fun fd ->
  match find_opt (fun buf -> not (Cstruct.is_empty buf)) bufs with
  | None -> 0
  | Some buf -> transfer_at ~write fd buf ~offset

(* A successful empty pipe message contributes no bytes to the stream.
   Both read paths return -1 when the peer closes its end. *)
let rec read_pipe read =
  match read () with
  | 0 -> read_pipe read
  | -1 -> 0
  | n -> n

external read_sync_pipe : Unix.file_descr -> Cstruct.buffer -> int -> int -> int = "caml_eio_windows_read_sync_pipe"

let read_cstruct kind fd (buf : Cstruct.t) =
  Eio_unix.Fd.use_exn "read" fd @@ fun raw ->
  match kind raw with
  | Socket -> Completion.await (fun port ~data -> Iocp.Event.recv port ~data raw buf)
  | (Pipe | Sync_pipe) when Cstruct.is_empty buf -> 0
  | Pipe ->
    read_pipe (fun () ->
        let c = Completion.await_result (fun port ~data -> Iocp.Event.read port ~data raw buf ~offset:0L) in
        if c.eof then -1 else Completion.transferred c)
  | Sync_pipe ->
    Serial.with_blocking_io raw (fun () ->
        Sync_io.run_with_check (fun check ->
            read_pipe (fun () -> check (); read_sync_pipe raw buf.buffer buf.off buf.len)))
  | File | Append_file -> sequential ~write:false ~append:false raw buf
  | (Pollable | Synchronous) as kind ->
    let read () = Eio_unix.run_in_systhread (fun () -> Unix.read_bigarray raw buf.buffer buf.off buf.len) in
    Serial.with_fd raw (fun () ->
        if kind = Pollable then Eio_unix.await_readable raw;
        read ())

let write_cstruct kind fd (buf : Cstruct.t) =
  Eio_unix.Fd.use_exn "write" fd @@ fun raw ->
  match kind raw with
  | Socket -> Completion.await (fun port ~data -> Iocp.Event.send port ~data raw buf)
  | _ when Cstruct.is_empty buf -> 0
  | Pipe -> Completion.await (fun port ~data -> Iocp.Event.write port ~data raw buf ~offset:0L)
  | (File | Append_file) as kind ->
    sequential ~write:true ~append:(kind = Append_file) raw buf
  | Pollable | Sync_pipe -> Serial.with_blocking_io raw (fun () -> Sync_io.write raw buf)
  | Synchronous -> Eio_unix.run_in_systhread (fun () -> Unix.single_write_bigarray raw buf.buffer buf.off buf.len)
