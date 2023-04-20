open Eio.Std

type t
(** A wrapper around a {!Unix.file_descr}. *)

(** {2 Creation} *)

val of_unix : sw:Switch.t -> ?blocking:bool -> ?seekable:bool -> close_unix:bool -> Unix.file_descr -> t
(** [of_unix ~sw ~close_unix fd] wraps [fd].

    @param sw Close [fd] automatically when [sw] is finished.
    @param blocking Indicates whether [fd] is in blocking mode.
                    If not given, [fd] is probed for its blocking state if needed.
    @param seekable The value to be returned by {!is_seekable}. Defaults to probing if needed.
    @param close_unix Whether {!close} also closes [fd] (this should normally be [true]). *)

(** {2 Using FDs} *)

val use : t -> (Unix.file_descr -> 'a) -> if_closed:(unit -> 'a) -> 'a
(** [use t fn ~if_closed] calls [fn wrapped_fd], ensuring that [wrapped_fd] will not be closed
    before [fn] returns.

    If [t] is already closed, it returns [if_closed ()] instead. *)

val use_exn : string -> t -> (Unix.file_descr -> 'a) -> 'a
(** [use_exn op t fn] calls [fn wrapped_fd], ensuring that [wrapped_fd] will not be closed
    before [fn] returns.

    If [t] is already closed, it raises an exception, using [op] as the name of the failing operation. *)

val use_exn_list : string -> t list -> (Unix.file_descr list -> 'a) -> 'a
(** [use_exn_list op fds fn] calls {!use_exn} on each FD in [fds], calling [fn wrapped_fds] on the results. *)

(** {2 Closing} *)

val close : t -> unit
(** [close t] marks [t] as closed, so that {!use} can no longer be used to start new operations.

    The wrapped FD will be closed once all current users of the FD have finished (unless [close_unix = false]).

    @raise Invalid_argument if [t] is closed by another fiber first. *)

val remove : t -> Unix.file_descr option
(** [remove t] marks [t] as closed, so that {!use} can no longer be used to start new operations.

    It then waits for all current users of the wrapped FD to finish using it, and then returns the FD.

    This operation suspends the calling fiber and so must run from an Eio fiber.
    It does not allow itself to be cancelled,
    since it takes ownership of the FD and that would be leaked if it aborted.

    Returns [None] if [t] is closed by another fiber first. *)

(** {2 Flags} *)

val is_blocking : t -> bool
(** [is_blocking t] returns the value of [blocking] passed to {!of_unix}.

    If not known, it first probes for it (and if the FD is already closed, returns [false]). *)

val is_seekable : t -> bool
(** [is_seekable t] returns the value of [seekable] passed to {!of_unix}.

    If not known, it first probes for it (and if the FD is already closed, returns [false]). *)

(** {2 Standard FDs} *)

val stdin : t
val stdout : t
val stderr : t

(** {2 Printing} *)

val pp : t Fmt.t
(** Displays the FD number. *)
