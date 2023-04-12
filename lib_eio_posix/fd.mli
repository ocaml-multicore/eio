(** A safe wrapper for {!Unix.file_descr}. *)

open Eio.Std

type t = Eio_unix.Fd.t
(** A wrapper around a {!Unix.file_descr}. *)

val of_unix : sw:Switch.t -> blocking:bool -> close_unix:bool -> Unix.file_descr -> t
(** [of_unix ~sw ~blocking ~close_unix fd] wraps [fd].

    @param sw Close [fd] automatically when [sw] is finished.
    @param blocking Indicates whether [fd] is in blocking mode.
                    Normally you should call [Unix.set_nonblock fd] first and pass [false] here.
    @param close_unix Whether {!close} also closes [fd] (this should normally be [true]). *)

val use_exn : string -> t -> (Unix.file_descr -> 'a) -> 'a
(** [use_exn op t fn] calls [fn wrapped_fd], ensuring that [wrapped_fd] will not be closed
    before [fn] returns.

    If [t] is already closed, it raises an exception, using [op] as the name of the failing operation. *)

val close : t -> unit
(** [close t] marks [t] as closed, so that {!use_exn} can no longer be used to start new operations.

    The wrapped FD will be closed once all current users of the FD have finished (unless [close_unix = false]).

    @raise Invalid_argument if [t] is closed by another fiber first. *)

val is_blocking : t -> bool
(** [is_blocking t] returns the value of [blocking] passed to {!of_unix}. *)

val stdin : t
val stdout : t
val stderr : t

val to_unix : [`Peek | `Take] -> t -> Unix.file_descr
(** [to_unix `Take t] closes [t] without closing the wrapped FD, which it returns to the caller once all operations on it have finished.

    [to_unix `Peek t] returns the wrapped FD directly. You must ensure that it is not closed while using it. *)

type has_fd = < fd : t >
(** Resources that have FDs are sub-types of [has_fd]. *)

val get_fd_opt : #Eio.Generic.t -> t option
(** [get_fd_opt r] returns the [t] being wrapped by a resource, if any.

    This just probes [r] using {!FD}. *)
