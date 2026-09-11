open Eio.Std

type rw_ty = [`Unix_fd | Eio.File.rw_ty]
type ro_ty = [`Unix_fd | Eio.File.ro_ty]

let open_type s = (s : rw_ty r :> [< rw_ty] r)

let import_rw ~sw ~close_unix fd =
  let fd = Fd.of_unix ~sw ~close_unix fd in
  (* The backend may set [fd] to be non-blocking.
     This is OK, because [Fd] checks for it lazily and we haven't asked it yet. *)
  open_type @@ Effect.perform (Private.Import_file fd)

let import_ro = import_rw
