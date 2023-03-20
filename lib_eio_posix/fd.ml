open Eio.Std

module Rcfd = Eio_unix.Private.Rcfd

type t = {
  fd : Rcfd.t;

  (* stdin, stdout and stderr are blocking, and so need special care.
     For these, we first wait for them to become e.g. readable and then hope
     that the read doesn't block. This may fail if multiple fibers try to read
     at the same time. We could check that it's still readable after being
     resumed, but that still won't work if multiple domains read at the same
     time. Same problem for writes. *)
  blocking : bool;
  close_unix : bool;                          (* Whether closing this also closes the underlying FD. *)
  mutable release_hook : Eio.Switch.hook;     (* Use this on close to remove switch's [on_release] hook. *)
}

let to_rcfd t = t.fd

let err_closed op = Invalid_argument (op ^ ": file descriptor used after calling close!")

let use_exn op t f =
  Rcfd.use t.fd f ~if_closed:(fun () -> raise (err_closed op))

let close t =
  Switch.remove_hook t.release_hook;
  if t.close_unix then (
    if not (Rcfd.close t.fd) then raise (err_closed "close")
  ) else (
    match Rcfd.remove t.fd with
    | Some _ -> ()
    | None -> raise (err_closed "close")
  )

let of_unix_no_hook ?(close_unix=true) ~blocking fd =
  { fd = Rcfd.make fd; blocking; close_unix; release_hook = Switch.null_hook }

let of_unix ~sw ~blocking ~close_unix fd =
  let t = of_unix_no_hook ~blocking ~close_unix fd in
  t.release_hook <- Switch.on_release_cancellable sw (fun () -> close t);
  t

let is_blocking t = t.blocking

let stdin = of_unix_no_hook ~blocking:true Unix.stdin
let stdout = of_unix_no_hook ~blocking:true Unix.stdout
let stderr= of_unix_no_hook ~blocking:true Unix.stderr

let to_unix op t =
  match op with
  | `Peek -> Rcfd.peek t.fd
  | `Take ->
    Switch.remove_hook t.release_hook;
    match Rcfd.remove t.fd with
    | Some fd -> fd
    | None -> raise (err_closed "to_unix")

type has_fd = < fd : t >

type _ Eio.Generic.ty += FD : t Eio.Generic.ty
let get_fd_opt t = Eio.Generic.probe t FD
