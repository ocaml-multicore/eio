open Eio.Std

val create : unit -> Eio.Domain_manager.ty r
(** [create ()] is a mock domain manager.

    When asked to run a new Eio domain, it just runs it in the parent domain.
    It runs the function in a context where {!id} is a fresh domain ID
    (assigned sequentially starting from 1). *)

val run : (Eio.Domain_manager.ty r -> 'a) -> 'a
(** [run fn] runs [fn dm], where [dm] is a new fake domain manager.
    It also runs {!with_domain_tracing} to display domain IDs in trace output.
 
    [fn] itself runs with {!id} set to "0". *)

val id : string Fiber.key
(** [id] is used to get or set the current fake domain's ID.
 
    This is used in traceln output. *)

val with_domain_tracing : (unit -> 'a) -> 'a
(** [with_domain_tracing fn] runs [fn ()] with a modified [traceln] function that
    prefixes the current {!id} (if any) to each trace message. *)
