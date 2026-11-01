(** Configuration settings to pass to {!Eio_main.run}.

    Unknown settings are ignored, so you can pass options for e.g. [Eio_linux.run] and they will
    be ignored if [Eio_posix] is used instead. *)

(** {2 eio_linux} *)

type fixed_buffer = {
  n_blocks : int;       (** The number of fixed buffer chunks to allocate. *)
  block_size : int;     (** The size of each fixed buffer chunk. *)
}

type linux = [
  | `Uring_queue_depth of int                   (** The number of jobs that can be submitted in one batch. *)

  | `Uring_fixed_buffer of fixed_buffer option  (** Use a fixed buffer to speed things up, if possible.
                                                    If there isn't enough locked memory available, this is ignored. *)

  | `Uring_polling_timeout of int option        (** How long (in ms) to continue polling when there's nothing to do.
                                                    If [None], polling isn't used. *)
]
(** Options recognised by the [Eio_linux] backend. *)

(** {2 Generic types} *)

type setting = linux    (** Currently, only eio_linux has any settings. *)

type t = setting list
