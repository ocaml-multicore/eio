(** The API is based on OCaml's [Semaphore.Counting].

    The difference is that when waiting for the semaphore this will switch to the next runnable fiber,
    whereas the stdlib one will block the whole domain.

    Semaphores are thread-safe and so can be shared between domains and used
    to synchronise between them. *)

type t
(** The type of counting semaphores. *)

val make : int -> t
(** [make n] returns a new counting semaphore, with initial value [n].
    The initial value [n] must be nonnegative.
    @raise Invalid_argument if [n < 0] *)

val release : t -> unit
(** [release t] increments the value of semaphore [t].
    If other fibers are waiting on [t], the one that has been waiting the longest is resumed.
    @raise Sys_error if the value of the semaphore would overflow [max_int] *)

val acquire : t -> unit
(** [acquire t] blocks the calling fiber until the value of semaphore [t]
    is not zero, then atomically decrements the value of [t] and returns. *)

val get_value : t -> int
(** [get_value t] returns the current value of semaphore [t]. *)
