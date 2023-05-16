(** Resources are typically operating-system provided resources such as open files
    and network sockets. However, they can also be pure OCaml resources (such as mocks)
    or wrappers (such as an encrypted flow that wraps an unencrypted OS flow).

    A resource's type shows which interfaces it supports. For example, a
    [[source | sink] t] is a resource that can be used as a source or a sink.

    If you are familiar with object types, this is roughly equivalent to the
    type [<source; sink>]. We avoid using object types here as some OCaml
    programmers find them confusing. *)

(** {2 Types} *)

type ('t, -'tags) handler
(** A [('t, 'tags) handler] can be used to look up the implementation for a type ['t].

    ['tags] is a phantom type to record which interfaces are supported.

    Internally, a handler is a set of {!type-binding}s. *)

type -'tags t = T : ('t * ('t, 'tags) handler) -> 'tags t (** *)
(** A resource is a pair of a value and a handler for it.

    Normally there will be convenience functions provided for using resources
    and you will not need to match on [T] yourself except when defining a new interface. *)

(** {2 Defining new interfaces}

    These types and functions can be used to define new interfaces that others
    can implement.

    When defining a new interface, you will typically provide:

    - The tags that indicate that the interface is supported (e.g. {!Flow.source_ty}).
    - A convenience type to match all sub-types easily (e.g. {!Flow.source}).
    - Functions allowing users to call the interface (e.g. {!Flow.single_read}).
    - A module to let providers implement the interface (e.g. {!Flow.Pi}).
*)

type ('t, 'iface, 'tag) pi = ..
(** A provider interface describes an interface that a resource can implement.

    - ['t] is the type of the resource itself.
    - ['iface] is the API that can be requested.
    - ['tag] is the tag (or tags) indicating that the interface is supported.

    For example, the value {!Close} (of type [(fd, fd -> unit, [> `Close]) pi]) can be
    used with a resource backed by an [fd], and which offers at least the
    [`Close] tag, to request its close function.
    Often, the API requested will be a module type, but it can be a single function
    as in this example.
*)

type _ binding = H : ('t, 'impl, 'tags) pi * 'impl -> 't binding (** *)
(** A binding [H (pi, impl)] says to use [impl] to implement [pi].

    For example: [H (Close, M.close)]. *)

val handler : 't binding list -> ('t, _) handler
(** [handler ops] is a handler that looks up interfaces using the assoc list [ops].

    For example [shutdown (module Foo)] is a handler that handles the [Close] and [Shutdown]
    interfaces for resources of type [Foo.t] by using the [Foo] module:

    {[
      let shutdown (type t) (module X : SHUTDOWN with type t = t) : (t, shutdown_ty) handler =
        handler [
          H (Close, X.close);
          H (Shutdown, (module X));
        ]
    ]}

    Be sure to give the return type explicitly, as this cannot be inferred.
*)

val bindings : ('t, _) handler -> 't binding list
(** [bindings (handler ops) = ops].

    This is useful if you want to extend an interface
    and you already have a handler for that interface. *)

val get : ('t, 'tags) handler -> ('t, 'impl, 'tags) pi -> 'impl
(** [get handler iface] uses [handler] to get the implementation of [iface].

    For example:
    {[
      let write (Resource.T (t, ops)) bufs =
        let module X = (val (Resource.get ops Sink)) in
        X.write t bufs
    ]}
*)

val get_opt : ('t, _) handler -> ('t, 'impl, _) pi -> 'impl option
(** [get_opt] is like {!get}, but the handler need not have a compatible type.
    Instead, this performs a check at runtime and returns [None] if the interface
    is not supported. *)

(** {2 Closing}

    Resources are usually attached to switches and closed automatically when the switch
    finishes. However, it can be useful to close them sooner in some cases. *)

type close_ty = [`Close]
type (_, _, _) pi += Close : ('t, 't -> unit, [> close_ty]) pi

val close : [> close_ty] t -> unit
(** [close t] marks the resource as closed. It can no longer be used after this.

    If [t] is already closed then this does nothing (it does not raise an exception).

    Note: if an operation is currently in progress when this is called then it is not
    necessarily cancelled, and any underlying OS resource (such as a file descriptor)
    might not be closed immediately if other operations are using it. Closing a resource
    only prevents new operations from starting. *)
