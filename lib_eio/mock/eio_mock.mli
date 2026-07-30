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

open Eio.Std

(** {2 Configuration} *)

(** Actions that can be performed by mock handlers. *)
module Action : sig
  type 'a t = [
    | `Return of 'a                     (** Immediately return a value *)
    | `Raise of exn                     (** Raise an exception *)
    | `Await of 'a Eio.Promise.or_exn   (** Wait for a promise to resolve *)
    | `Yield_then of 'a t               (** Call {!Eio.Fiber.yield}, then perform an action *)
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

  type ty = [`Generic | `Mock] Eio.Net.stream_socket_ty
  type t = ty r

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
  type t = [`Generic | `Mock] Eio.Net.ty r

  type listening_socket = [`Generic | `Mock] Eio.Net.listening_socket_ty r

  val make : string -> t
  (** [make label] is a new mock network. *)

  val on_connect : t -> _ Eio.Net.stream_socket Handler.actions -> unit
  (** [on_connect t actions] configures what to do when a client tries to connect somewhere. *)

  val on_listen : t -> _ Eio.Net.listening_socket Handler.actions -> unit
  (** [on_listen t actions] configures what to do when a server starts listening for incoming connections. *)

  val on_datagram_socket : t -> _ Eio.Net.datagram_socket Handler.actions -> unit
  (** [on_datagram_socket t actions] configures how to create datagram sockets. *)

  val on_getaddrinfo : t -> Eio.Net.Sockaddr.t list Handler.actions -> unit

  val on_getnameinfo : t -> (string * string) Handler.actions -> unit

  val listening_socket :
    ?listening_addr:Eio.Net.Sockaddr.stream -> string -> listening_socket
  (** [listening_socket label] can be configured to provide mock connections.

      If [listening_addr] is not provided, a dummy value will be reported. *)

  val on_accept :
    listening_socket ->
    (Flow.t * Eio.Net.Sockaddr.stream) Handler.actions ->
    unit
  (** [on_accept socket actions] configures how to respond when the server calls "accept". *)
end

(** Mock {!Eio.Fs} directories. *)
module Dir : sig
  type ty = [ Eio.Fs.dir_ty | `Close | `Mock ]
  type t = ty r

  val make : ?syntax:[`Posix | `Windows] -> string -> t
  (** [make label] is a new mock directory.
      @param syntax The path syntax to use when joining and splitting paths (default [`Posix]). *)

  val on_open_in : t -> _ Eio.File.ro Handler.actions -> unit
  (** [on_open_in t actions] configures what to return when opening a file for reading. *)

  val on_open_out : t -> _ Eio.File.rw Handler.actions -> unit
  (** [on_open_out t actions] configures what to return when opening a file for writing. *)

  val on_open_subtree : t -> [> `Close | Eio.Fs.dir_ty] r Handler.actions -> unit
  (** [on_open_subtree t actions] configures what to return when opening a sub-directory.
      Typically the actions return further mock directories. *)

  val on_read_dir : t -> string list Handler.actions -> unit
  (** [on_read_dir t actions] configures the entries to report when listing a directory.
      This is used by {!Eio.Path.read_dir} and by default also by {!on_dir_entries}. *)

  val on_dir_entries : t -> (Eio.File.Stat.kind * string) list Handler.actions -> unit
  (** [on_dir_entries t actions] configures the entries to report from directory
      list operations. By defaul, this runs the {!on_read_dir} handler and reports
      every entry's kind as [`Unknown]. *)

  val on_stat : t -> Eio.File.Stat.t Handler.actions -> unit
  (** [on_stat t actions] configures the results of {!Eio.Path.stat}. *)

  val on_read_link : t -> string Handler.actions -> unit
  (** [on_read_link t actions] configures the results of {!Eio.Path.read_link}. *)

  val on_mkdir : t -> unit Handler.actions -> unit
  (** [on_mkdir t actions] configures what to do when {!Eio.Path.mkdir} is called.
      By default it just returns unit; use this to simulate faults. *)

  val on_unlink : t -> unit Handler.actions -> unit
  (** [on_unlink t actions] configures what to do when {!Eio.Path.unlink} is called.
      By default it just returns unit; use this to simulate faults. *)

  val on_rmdir : t -> unit Handler.actions -> unit
  (** [on_rmdir t actions] configures what to do when {!Eio.Path.rmdir} is called.
      By default it just returns unit; use this to simulate faults. *)

  val on_rename : t -> unit Handler.actions -> unit
  (** [on_rename t actions] configures what to do when {!Eio.Path.rename} is called.
      By default it just returns unit; use this to simulate faults. *)

  val on_symlink : t -> unit Handler.actions -> unit
  (** [on_symlink t actions] configures what to do when {!Eio.Path.symlink} is called.
      By default it just returns unit; use this to simulate faults. *)

  val on_chmod : t -> unit Handler.actions -> unit
  (** [on_chmod t actions] configures what to do when {!Eio.Path.chmod} is called.
      By default it just returns unit; use this to simulate faults. *)

  val on_chown : t -> unit Handler.actions -> unit
  (** [on_chown t actions] configures what to do when {!Eio.Path.chown} is called.
      By default it just returns unit; use this to simulate faults. *)
end

(** A mock {!Eio.Time} clock for testing timeouts. *)
module Clock = Clock

(** A mock {!Eio.Domain_manager} that runs everything in a single domain. *)
module Domain_manager = Domain_manager

(** {2 Backend for mocks}

    The mocks can be used with any backend, but if you don't need any IO then you can use this one
    to avoid a dependency on eio_main. *)

module Backend = Backend

(** {2 Mock errors} *)

type Eio.Exn.Backend.t += Simulated_failure
(** A fake error code you can use for simulated faults. *)
