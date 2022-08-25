(** Mocks for testing.

    When testing an Eio program it is often convenient to use mock resources rather than real OS-provided ones.
    This allows precise control over the test, such as adding delays or simulated faults.
    You can always just implement the various Eio types directly,
    but this module provides some convenient pre-built mocks, and some helpers for creating your own mocks.

    Mocks typically use {!Eio.traceln} to record how they were used.
    This output can be recorded and compared against a known-good copy using e.g.
    {{:https://github.com/realworldocaml/mdx}ocaml-mdx}.

    Mocks may require configuration.
    For example, a source flow needs to know what data to return when the application reads from it.
    This can be done using the various [on_*] functions. For example:

    {[
      let stdin = Eio_mock.Flow.make "stdin" in
      let stdout = Eio_mock.Flow.make "stdout" in
      Eio_mock.Flow.on_read stdin [
        `Return "chunk1";
        `Return "chunk2";
        `Raise End_of_file
      ];
      Eio.Flow.copy stdin stdout
    ]}

    This will produce:

    {[
      +stdin: read "chunk1"
      +stdout: wrote "chunk1"
      +stdin: read "chunk2"
      +stdout: wrote "chunk2"
    ]}
*)

(** {2 Configuration} *)

(** Actions that can be performed by mock handlers. *)
module Action : sig
  type 'a t = [
    | `Return of 'a                     (** Immediately return a value *)
    | `Raise of exn                     (** Raise an exception *)
    | `Await of 'a Eio.Promise.or_exn   (** Wait for a promise to resolve *)
    | `Yield_then of 'a t               (** Call {!Fiber.yield}, then perform an action *)
    | `Run of unit -> 'a                (** Run any code you like. *)
  ]

  val run : 'a t -> 'a
  (** [run t] performs action [t] and returns the result. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [run (map f t) = f (run t)]. *)
end

(** Control how a mock responds.

    This module is mostly useful when writing custom mocks.
    Individual mocks usually provide convenience wrappers around this. *)
module Handler : sig
  type 'a t
  (** A handler that provides values of type ['a]. *)

  type 'a actions = 'a Action.t list

  val make : 'a Action.t -> 'a t
  (** [make default_action] is a new handler that initially always runs [default_action]. *)

  val set_handler : 'a t -> (unit -> 'a) -> unit
  (** [set_handler t fn] sets (replaces) the function to be called whenever the handler is run. *)

  val seq : 'a t -> 'a actions -> unit
  (** [seq t actions] sets a handler function that performs the next action in [actions] on each call.
      When there are no more actions, it runs the default handler. *)

  val run : 'a t -> 'a
  (** [run t] is used by mocks to run their handlers. *)

  val run_default_action : 'a t -> 'a
  (** [run_default_action t] runs the default handler passed to {!make}. *)
end

(** {2 Pre-defined mocks} *)

(** Mock {!Eio.Flow} sources and sinks. *)
module Flow : sig
  type copy_method = [
    | `Read_into                (** Use the source's [read_into] method (the default). *)
    | `Read_source_buffer       (** Use the {!Eio.Flow.Read_source_buffer} optimisation. *)
  ]

  type t = <
    Eio.Flow.two_way;
    Eio.Flow.close;
    on_read : string Handler.t;
    on_copy_bytes : int Handler.t;
    set_copy_method : copy_method -> unit;
    attach_to_switch : Eio.Switch.t -> unit;
  >

  val make : ?pp:string Fmt.t -> string -> t
  (** [make label] is a mock Eio flow.
      It can be used as a source, sink, or two-way flow.
      @param pp Printer to use to display the data. *)

  val on_read : t -> string Handler.actions -> unit
  (** [on_read t actions] configures the values to return from the mock's [read] function. *)

  val on_copy_bytes : t -> int Handler.actions -> unit
  (** [on_copy_bytes t actions] configures the number of bytes to copy in each iteration. *)

  val set_copy_method : t -> copy_method -> unit
  (** [set_copy_method t m] configures [t] to use the given method to read from
      a source during a copy operation. *)
end

(** Mock {!Eio.Net} networks and sockets. *)
module Net : sig
  type t = <
    Eio.Net.t;
    on_listen : Eio.Net.listening_socket Handler.t;
    on_connect : <Eio.Net.stream_socket; Eio.Flow.close> Handler.t;
    on_datagram_socket : <Eio.Net.datagram_socket; Eio.Flow.close> Handler.t;
    on_getaddrinfo : Eio.Net.Sockaddr.t list Handler.t;
    on_getnameinfo : (string * string) Handler.t;
  >

  type listening_socket = <
    Eio.Net.listening_socket;
    on_accept : (Flow.t * Eio.Net.Sockaddr.stream) Handler.t;
  >

  val make : string -> t
  (** [make label] is a new mock network. *)

  val on_connect : t -> <Eio.Net.stream_socket; Eio.Flow.close; ..> Handler.actions -> unit
  (** [on_connect t actions] configures what to do when a client tries to connect somewhere. *)

  val on_listen : t -> #Eio.Net.listening_socket Handler.actions -> unit
  (** [on_listen t actions] configures what to do when a server starts listening for incoming connections. *)

  val on_datagram_socket : t -> <Eio.Net.datagram_socket; Eio.Flow.close; ..> Handler.actions -> unit
  (** [on_datagram_socket t actions] configures how to create datagram sockets. *)

  val on_getaddrinfo : t -> Eio.Net.Sockaddr.t list Handler.actions -> unit

  val on_getnameinfo : t -> (string * string) Handler.actions -> unit

  val listening_socket : string -> listening_socket
  (** [listening_socket label] can be configured to provide mock connections. *)

  val on_accept :
    listening_socket ->
    (Flow.t * Eio.Net.Sockaddr.stream) Handler.actions ->
    unit
  (** [on_accept socket actions] configures how to respond when the server calls "accept". *)
end

(** {2 Backend for mocks}

    The mocks can be used with any backend, but if you don't need any IO then you can use this one
    to avoid a dependency on eio_main. *)

module Backend = Backend
