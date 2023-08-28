type ty = [`Domain_mgr]
type 'a t = ([> ty] as 'a) Resource.t

val run : _ t -> (unit -> 'a) -> 'a
(** [run t f] runs [f ()] in a newly-created domain and returns the result.

    Other fibers in the calling domain can run in parallel with the new domain.

    Warning: [f] must only access thread-safe values from the calling domain,
    but this is not enforced by the type system.

    If the calling fiber is cancelled, this is propagated to the spawned domain. *)

val run_raw : _ t -> (unit -> 'a) -> 'a
(** [run_raw t f] is like {!run}, but does not run an event loop in the new domain,
    and so cannot perform IO, fork fibers, etc. *)

(** {2 Provider Interface} *)

module Pi : sig
  module type MGR = sig
    type t

    val run : t -> (cancelled:exn Promise.t -> 'a) -> 'a
    (** [run t fn] runs [fn ~cancelled] in a new domain.

        If the calling fiber is cancelled, [cancelled] becomes resolved to the {!Cancel.Cancelled} exception.
        [fn] should cancel itself in this case. *)

    val run_raw : t -> (unit -> 'a) -> 'a
  end

  type (_, _, _) Resource.pi +=
    | Mgr : ('t, (module MGR with type t = 't), [> ty]) Resource.pi

  val mgr : (module MGR with type t = 't) -> ('t, ty) Resource.handler
end
