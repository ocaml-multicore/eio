type 'a ty = ..
(** An ['a ty] is a query for a feature of type ['a]. *)

class type t = object
  method probe : 'a. 'a ty -> 'a option
end

val probe : #t -> 'a ty -> 'a option
(** [probe t feature] checks whether [t] supports [feature].
    This is mostly for internal use.
    For example, {!Eio_unix.FD.peek_opt} uses this to get the underlying Unix file descriptor from a flow. *)

(** {2 Closing}

    Resources are usually attached to switches and closed automatically when the switch
    finishes. However, it can be useful to close them sooner in some cases. *)

class type close = object
  method close : unit
end

val close : #close -> unit
(** [close t] marks the resource as closed. It can no longer be used after this.

    If [t] is already closed then this does nothing (it does not raise an exception).

    Note: if an operation is currently in progress when this is called then it is not
    necessarily cancelled, and any underlying OS resource (such as a file descriptor)
    might not be closed immediately if other operations are using it. Closing a resource
    only prevents new operations from starting. *)
