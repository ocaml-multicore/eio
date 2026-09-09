open Eio.Std

type rw_ty = [`Unix_fd | Eio.File.rw_ty]
type ro_ty = [`Unix_fd | Eio.File.ro_ty]

val import_rw : sw:Switch.t -> close_unix:bool -> Unix.file_descr -> [< rw_ty ] r
(** [import_rw ~sw ~close_unix fd] is a read/write Eio file that uses [fd].

    The file resource will be closed when [sw] finishes.

    The backend takes ownership of [fd] and may change whether it is
    non-blocking.

    The [close_unix] and [sw] arguments are passed to {!Fd.of_unix}. *)

val import_ro : sw:Switch.t -> close_unix:bool -> Unix.file_descr -> [< ro_ty ] r
(** [import_ro] is like {!import_rw}, but casts the result to be read-only. *)
