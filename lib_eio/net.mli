(** Example:
    {[
      let addr = `Tcp (Ipaddr.V4.loopback, 8080)

      let http_get ~net ~stdout addr =
        Switch.run @@ fun sw ->
        let flow = Net.connect ~sw net addr in
        Flow.copy_string "GET / HTTP/1.0\r\n\r\n" flow;
        Flow.shutdown flow `Send;
        Flow.copy flow stdout
      ]}
*)

exception Connection_reset of exn

(** IP addresses. *)
module Ipaddr : sig
  type 'a t = private string
  (** The raw bytes of the IP address.
      It is either 4 bytes long (for an IPv4 address) or
      16 bytes long (for IPv6). *)

  (** IPv4 addresses. *)
  module V4 : sig
    val any : [> `V4] t
    (** A special IPv4 address, for use only with [listen], representing
        all the Internet addresses that the host machine possesses. *)

    val loopback : [> `V4] t
    (** A special IPv4 address representing the host machine ([127.0.0.1]). *)
  end

  (** IPv6 addresses. *)
  module V6 : sig
    val any : [> `V6] t
    (** A special IPv6 address, for use only with [listen], representing
        all the Internet addresses that the host machine possesses. *)

    val loopback : [> `V6] t
    (** A special IPv6 address representing the host machine ([::1]). *)
  end

  val pp : [< `V4 | `V6] t Fmt.t
  (** [pp] formats IP addresses.
      For IPv6 addresses, it follows {{:http://tools.ietf.org/html/rfc5952}}. *)

  type v4v6 = [`V4 | `V6] t

  val fold :
    v4:([> `V4] t -> 'a) -> 
    v6:([> `V6] t -> 'a) ->
    [< `V4 | `V6] t ->
    'a
  (** [fold ~v4 ~v6 t] is [v4 t] if [t] is an IPv4 address, or [v6 t] if it's an IPv6 address. *)

  val of_raw : string -> v4v6
  (** [of_raw addr] casts [addr] to an IP address.
      @raise Invalid_argument if it is not 4 or 16 bytes long. *)
end

(** Network addresses. *)
module Sockaddr : sig
  type stream = [
    | `Unix of string
    | `Tcp of Ipaddr.v4v6 * int
  ]
  (** Socket addresses that we can build a {! Flow.two_way} for i.e. stream-oriented
      protocols. *)

  type datagram = [
    | `Udp of Ipaddr.v4v6 * int
  ]
  (** Socket addresses that are message-oriented. *)

  type t = [ stream | datagram ]

  val pp : Format.formatter -> [< t] -> unit
end

(** {2 Provider Interfaces} *)

class virtual socket : object
  inherit Generic.t
  method virtual close : unit
end

class virtual stream_socket : object
  inherit socket
  inherit Flow.two_way
end

class virtual datagram_socket : object
  inherit socket
  method virtual send : Sockaddr.datagram -> Cstruct.t -> unit
  method virtual recv : Cstruct.t -> Sockaddr.datagram * int
end

class virtual listening_socket : object
  inherit socket
  method virtual accept : sw:Switch.t -> stream_socket * Sockaddr.stream
end

class virtual t : object
  method virtual listen : reuse_addr:bool -> reuse_port:bool -> backlog:int -> sw:Switch.t -> Sockaddr.stream -> listening_socket
  method virtual connect : sw:Switch.t -> Sockaddr.stream -> stream_socket
  method virtual datagram_socket : sw:Switch.t -> Sockaddr.datagram -> datagram_socket
end

(** {2 Out-bound Connections} *)

val connect : sw:Switch.t -> #t -> Sockaddr.stream -> stream_socket
(** [connect ~sw t addr] is a new socket connected to remote address [addr].

    The new socket will be closed when [sw] finishes, unless closed manually first. *)

(** {2 Incoming Connections} *)

val listen : ?reuse_addr:bool -> ?reuse_port:bool -> backlog:int -> sw:Switch.t -> #t -> Sockaddr.stream -> listening_socket
(** [listen ~sw ~backlog t addr] is a new listening socket bound to local address [addr].

    The new socket will be closed when [sw] finishes, unless closed manually first.

    For (non-abstract) Unix domain sockets, the path will be removed afterwards.

    @param backlog The number of pending connections that can be queued up (see listen(2)).
    @param reuse_addr Set the {!Unix.SO_REUSEADDR} socket option.
                      For Unix paths, also remove any stale left-over socket.
    @param reuse_port Set the {!Unix.SO_REUSEPORT} socket option. *)

val accept :
  sw:Switch.t ->
  #listening_socket ->
  stream_socket * Sockaddr.stream
(** [accept ~sw socket] waits until a new connection is ready on [socket] and returns it.

    The new socket will be closed automatically when [sw] finishes, if not closed earlier.
    If you want to handle multiple connections, consider using {!accept_fork} instead. *)

val accept_fork :
  sw:Switch.t ->
  #listening_socket ->
  on_error:(exn -> unit) ->
  (stream_socket -> Sockaddr.stream -> unit) ->
  unit
(** [accept_fork socket fn] accepts a connection and handles it in a new fiber.

    After accepting a connection to [socket], it runs [fn flow client_addr] in a new fiber.

    [flow] will be closed automatically when [fn] returns, if not already closed by then. *)

val accept_sub :
  sw:Switch.t ->
  #listening_socket ->
  on_error:(exn -> unit) ->
  (sw:Switch.t -> stream_socket -> Sockaddr.stream -> unit) ->
  unit
[@@deprecated "Use accept_fork instead"]

(** {2 Datagram Sockets} *)

val datagram_socket : sw:Switch.t -> #t -> Sockaddr.datagram -> datagram_socket
(** [datagram_socket ~sw t addr] creates a new datagram socket that data can be sent to
    and received from. The new socket will be closed when [sw] finishes. *)

val send : datagram_socket -> Sockaddr.datagram -> Cstruct.t -> unit
(** [send sock addr buf] sends the data in [buf] to the address [addr] using the 
    the datagram socket [sock]. *)

val recv : datagram_socket -> Cstruct.t -> Sockaddr.datagram * int
(** [recv sock buf] receives data from the socket [sock] putting it in [buf]. The number of bytes received is 
    returned along with the sender address and port. If the [buf] is too small then excess bytes may be discarded
    depending on the type of the socket the message is received from. *)

(** {2 Closing} *)
val close : #socket -> unit
(** [close t] marks the socket as closed. It can no longer be used after this. *)
