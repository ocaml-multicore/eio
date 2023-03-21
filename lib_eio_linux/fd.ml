open Eio.Std

module Rcfd = Eio_unix.Private.Rcfd
module Ctf = Eio.Private.Ctf

type t = {
  fd : Rcfd.t;
  seekable : bool;
  close_unix : bool;                          (* Whether closing this also closes the underlying FD. *)
  mutable release_hook : Eio.Switch.hook;     (* Use this on close to remove switch's [on_release] hook. *)
}

let err_closed op = Invalid_argument (op ^ ": file descriptor used after calling close!")

let use t f ~if_closed = Rcfd.use t.fd f ~if_closed

let use_exn op t f =
  Rcfd.use t.fd f ~if_closed:(fun () -> raise (err_closed op))

let rec use_exn_list op xs k =
  match xs with
  | [] -> k []
  | x :: xs ->
    use_exn op x @@ fun x ->
    use_exn_list op xs @@ fun xs ->
    k (x :: xs)

let is_open t = Rcfd.is_open t.fd

let close t =
  Ctf.label "close";
  Switch.remove_hook t.release_hook;
  if t.close_unix then (
    if not (Rcfd.close t.fd) then raise (err_closed "close")
  ) else (
    match Rcfd.remove t.fd with
    | Some _ -> ()
    | None -> raise (err_closed "close")
  )

let is_seekable fd =
  match Unix.lseek fd 0 Unix.SEEK_CUR with
  | (_ : int) -> true
  | exception Unix.Unix_error(Unix.ESPIPE, "lseek", "") -> false

let to_unix op t =
  match op with
  | `Peek -> Rcfd.peek t.fd
  | `Take ->
    Switch.remove_hook t.release_hook;
    match Rcfd.remove t.fd with
    | Some fd -> fd
    | None -> raise (err_closed "to_unix")

let of_unix_no_hook ~seekable ~close_unix fd =
  { fd = Rcfd.make fd; seekable; close_unix; release_hook = Switch.null_hook }

let of_unix ~sw ~seekable ~close_unix fd =
  let t = of_unix_no_hook ~seekable ~close_unix fd in
  t.release_hook <- Switch.on_release_cancellable sw (fun () -> close t);
  t

let uring_file_offset t =
  if t.seekable then Optint.Int63.minus_one else Optint.Int63.zero

let file_offset t = function
  | Some x -> `Pos x
  | None when t.seekable -> `Seekable_current
  | None -> `Nonseekable_current
