type 'a ty = ..
(** An ['a ty] is a query for a feature of type ['a]. *)

class type t = object
  method probe : 'a. 'a ty -> 'a option
end

val probe : #t -> 'a ty -> 'a option
(** [probe t feature] checks whether [t] supports [feature].
    This is mostly for internal use.
    For example, {!Eio_unix.FD.peek_opt} uses this to get the underlying Unix file descriptor from a flow. *)
