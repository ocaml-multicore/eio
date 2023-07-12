type t

(** Creates a new workpool with [domain_count].

    [domain_concurrency] is the maximum number of jobs that each domain can run at a time.

    [transient] (default: true). When true, the workpool will not block the [~sw] Switch from completing.
      When false, you must call [terminate] to release the [~sw] Switch. *)
val create :
  sw:Switch.t ->
  domain_count:int ->
  domain_concurrency:int ->
  ?transient:bool ->
  _ Domain_manager.t ->
  t

(** Run a job on this workpool. It is placed at the end of the queue. *)
val submit : t -> (unit -> 'a) -> ('a, exn) result

(** Same as [submit] but raises if the job failed. *)
val submit_exn : t -> (unit -> 'a) -> 'a

(** Same as [submit] but returns immediately, without blocking. *)
val submit_fork : sw:Switch.t -> t -> (unit -> 'a) -> ('a, exn) result Promise.t

(** Waits for all running jobs to complete, then returns.
    No new jobs are started, even if they were already enqueued.
    To abort all running jobs instead of waiting for them, call [Switch.fail] on the Switch used to create this workpool *)
val terminate : t -> unit

(** Returns true if the [terminate] function has been called on this workpool.
    Also returns true if the workpool has fully terminated. *)
val is_terminating : t -> bool

(** Returns true if the [terminate] function has been called on this workpool AND
    the workpool has fully terminated (all running jobs have completed). *)
val is_terminated : t -> bool
