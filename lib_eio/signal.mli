(** Signals

    Signals in EIO work on a subscription basis. A {!sigbox} is
    subscribed to a specific {!signum} via [subscribe], which can in
    turn be waited on with [wait].

    When a signal arrives, all {!sigbox}es subscribed to the
    corresponding signal are populated and [wait] returns.

    Signals are buffered but not indefinately, the size of the buffer
    and the number of signals queued depend on the backend implementation.

    There can be multiple subscriptions for the same {!signum}, even
    across different domains, but each individual {!sigbox} is local
    to its domain and tied to a {!Switch.t}
*)

(** Example:
    {[
      Switch.run @@ fun sw ->
      let sighupbox = Signal.(subscribe ~sw sighup) in
      while !run do
        Signal.wait sighupbox;
        traceln "Got sighup";
        daemon_reconfigure ();
      done;
      Signal.unsubscribe sighupbox
    ]}

    {[
      Fiber.first
        (fun () -> Signal.(wait_one sigint))
        (fun () -> Time.sleep clock 1.0)
    ]}
*)

type sigbox
(** Stores signals that can be [wait]ed on, each {!sigbox} listens to
    one {!signum}, you can have multiple {!sigbox} for the same
    {!signum}, and in multiple domains.

*)

module Private : sig
  type _ Effect.t +=
    | Subscribe : (int * unit Stream.t) -> unit Effect.t
    | Unsubscribe : (int * unit Stream.t) -> unit Effect.t
    | Publish : int -> unit Effect.t
end

type signum = private int

val subscribe : sw:Switch.t -> signum -> sigbox
(** Create a subscription for waiting on signal {!signum}. *)

val unsubscribe : sigbox -> unit
(** Remove a subscription, possibly restoring the orignal {!signum} behaviour. *)

val publish : signum -> unit
(** Raise a signal, like kill. **)

val wait : sigbox -> unit
(** Wait for signals, it is an error to [wait] on an unsubscribed {!sigbox}. *)

val wait_one : signum -> unit
(** Like [wait] but only for a single occurrence of {!signum}. *)

val is_pending : sigbox -> bool
(** false if [wait] would block. *)

val signum_to_int : signum -> int
(** The system's idea of {!signum}, always a positive number, unlike signals from {!Sys}. *)

(** Signals, some are optional since not every system supports those. *)
val sighup : signum
val sigint : signum
val sigquit : signum
val sigill : signum
val sigtrap : signum
val sigabrt : signum
val sigemt_opt : signum option
val sigfpe : signum
val sigkill : signum
val sigbus : signum
val sigsegv : signum
val sigsys : signum
val sigpipe : signum
val sigalrm : signum
val sigterm : signum
val sigurg : signum
val sigstop : signum
val sigtstp : signum
val sigcont : signum
val sigchld : signum
val sigttin : signum
val sigttou : signum
val sigio : signum
val sigxcpu : signum
val sigxfsz : signum
val sigvtalrm : signum
val sigprof : signum
val sigwinch : signum
val siginfo_opt : signum option
val sigusr1 : signum
val sigusr2 : signum
val sigthr_opt : signum option
val nsig : int
