(** This is useful to manage a collection of resources where creating new ones is expensive
    and so you want to reuse them where possible.

    Example:

    {[
      let buffer_pool = Eio.Pool.create 10 (fun () -> Bytes.create 1024) in
      Eio.Pool.use buffer_pool (fun buf -> ...)
    ]}

    Note: If you just need to limit how many resources are in use, it is simpler to use {!Eio.Semaphore} instead.
*)

type 'a t

val create :
  ?validate:('a -> bool) ->
  ?dispose:('a -> unit) ->
  int ->
  (unit -> 'a) ->
  'a t
(** [create n alloc] is a fresh pool which allows up to [n] resources to be live at a time.
    It uses [alloc] to create new resources as needed.
    If [alloc] raises an exception then that use fails, but future calls to {!use} will retry.

    The [alloc] function is called in the context of the fiber trying to use the pool.

    You should take care about handling cancellation in [alloc], since resources are typically
    attached to a switch with the lifetime of the pool, meaning that if [alloc] fails then they won't
    be freed automatically until the pool itself is finished.

    @param validate If given, this is used to check each resource before using it.
                    If it returns [false], the pool removes it with [dispose] and then allocates a fresh resource.
    @param dispose Used to free resources rejected by [validate].
                   If it raises, the exception is passed on to the user,
                   but resource is still considered to have been disposed. *)

val use : 'a t -> ?never_block:bool -> ('a -> 'b) -> 'b
(** [use t fn] waits for some resource [x] to be available and then runs [f x].
    Afterwards (on success or error), [x] is returned to the pool.

    @param never_block If [true] and the pool has reached maximum capacity,
                       then a fresh resource is created to ensure that this [use]
                       call does not wait for a resource to become available.
                       This resource is immediately disposed after [f x] returns.
    *)
