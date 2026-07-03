(** Pseudoterminal (PTY) support.

    A pseudoterminal is a pair of connected file descriptors emulating a
    terminal. The {e pseudoterminal device} is used by a controlling program
    such as a terminal emulator, while the {e terminal device} is used by
    a child process as its controlling terminal. *)

open Eio.Std

type t
(** A connected pseudoterminal pair. *)

val open_pty : sw:Switch.t -> unit -> t
(** [open_pty ~sw ()] allocates a new pseudoterminal pair.

    Both file descriptors are closed when [sw] finishes. The {!pty} end
    is non-blocking; the {!tty} end is blocking and so suitable as the
    child's controlling terminal.

    Not a multi-domain-safe function on some platforms without reentrant
    [ptsname] support.

    @raise Unix.Unix_error if the pseudoterminal cannot be created. *)

val pty : t -> Fd.t
(** [pty t] is the pseudoterminal-device end. *)

val tty : t -> Fd.t
(** [tty t] is the terminal-device end. *)

val name : t -> string
(** [name t] is the path of the terminal device. *)

val source : t -> Types.source_ty r
(** [source t] reads the output the child writes to terminal. *)

val sink : t -> Types.sink_ty r
(** [sink t] writes input for the child to read from its terminal. *)

(** Terminal window dimensions. *)
type winsize = {
  rows : int;     (** Height of the terminal in character rows. *)
  cols : int;     (** Width of the terminal in character columns. *)
  xpixel : int;   (** Width in pixels ([0] if unknown). *)
  ypixel : int;   (** Height in pixels ([0] if unknown). *)
}

val get_window_size : Fd.t -> winsize
(** [get_window_size fd] returns the window size of terminal [fd].

    @raise Unix.Unix_error if [fd] is not a terminal. *)

val set_window_size : Fd.t -> winsize -> unit
(** [set_window_size fd ws] sets the window size of terminal [fd].

    Setting it on the {!pty} end updates the terminal and delivers [SIGWINCH] to
    the foreground process group attached to the terminal.

    @raise Unix.Unix_error if [fd] is not a terminal. *)

(** Terminal attributes control.
    These raise [Unix.Unix_error] if [fd] is not a terminal. *)
module Tc : sig
  val getattr : Fd.t -> Unix.terminal_io
  (** [getattr fd] returns the current terminal attributes of [fd].
      See {!Unix.tcgetattr}. *)

  val setattr : Fd.t -> Unix.setattr_when -> Unix.terminal_io -> unit
  (** [setattr fd when_ attr] sets the terminal attributes of [fd].

      With [TCSADRAIN] or [TCSAFLUSH] the change waits for pending output to
      drain in the current fiber.
      See {!Unix.tcsetattr}. *)

  val sendbreak : Fd.t -> int -> unit
  (** [sendbreak fd duration] sends a break condition on [fd].

      This blocks the current fiber while the break is transmitted.
      See {!Unix.tcsendbreak}. *)

  val drain : Fd.t -> unit
  (** [drain fd] waits until all output written to [fd] has been transmitted.

      This blocks the current fiber until the output drains.
      See {!Unix.tcdrain}. *)

  val flush : Fd.t -> Unix.flush_queue -> unit
  (** [flush fd queue] discards pending input and/or output on [fd].
      See {!Unix.tcflush}. *)

  val flow : Fd.t -> Unix.flow_action -> unit
  (** [flow fd action] suspends or resumes transmission/reception on [fd].
      See {!Unix.tcflow}. *)
end
