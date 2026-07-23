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

open Std

type connection_failure =
  | Refused of Exn.Backend.t
  | Timeout

module Getaddrinfo_error : sig
  (** Error codes returned by getaddrinfo(3) *)

  type t =
    | UNKNOWN
    | ADDRFAMILY
    | AGAIN
    | BADFLAGS
    | BADHINTS
    | FAIL
    | FAMILY
    | MEMORY
    | NODATA
    | NONAME
    | OVERFLOW
    | PROTOCOL
    | SERVICE
    | SOCKTYPE

  val to_tag : t -> string
  (** [to_tag t] is the constructor name. e.g. [to_tag NONAME = "NONAME"]. *)

  val to_message : t -> string
  (** [to_message t] returns a string representation of [t], like gai_strerror(3). *)
end

type error =
  | Connection_reset of Exn.Backend.t
    (** This is a wrapper for epipe, econnreset and similar errors.
        It indicates that the flow has failed, and data may have been lost. *)
  | Connection_failure of connection_failure
  | Address_lookup_failed of Getaddrinfo_error.t
  | Invalid_option (** Socket option not supported. *)

type Exn.err += E of error

val err : error -> exn
(** [err e] is [Eio.Exn.create (Net e)] *)

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

  (** {2 Interoperability}

  To convert to or from OCaml Unix addresses, use {!Eio_unix.Net.Ipaddr}.

  To interoperate with the {{:https://opam.ocaml.org/packages/ipaddr/} ipaddr} library:
  - [Ipaddr.to_octets ipaddr_ip |> Eio.Net.Ipaddr.of_raw]
  - [Ipaddr.of_octets_exn (eio_ip :> string)] *)

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
    | `Unix of string
  ]
  (** Socket addresses that are message-oriented. *)

  type t = [ stream | datagram ]

  val pp : Format.formatter -> [< t] -> unit
end

(** {2 Types} *)

type socket_ty = [`Socket | `Close]
type 'a socket = ([> socket_ty] as 'a) r

type 'tag stream_socket_ty = [`Stream | `Platform of 'tag | `Shutdown | socket_ty | Flow.source_ty | Flow.sink_ty]
type 'a stream_socket = 'a r
  constraint 'a = [> [> `Generic] stream_socket_ty]

type 'tag listening_socket_ty = [ `Accept | `Platform of 'tag | socket_ty]
type 'a listening_socket = 'a r
  constraint 'a = [> [> `Generic] listening_socket_ty]

type 'a connection_handler = 'a stream_socket -> Sockaddr.stream -> unit
(** A [_ connection_handler] handles incoming connections from a listening socket. *)

type 'tag datagram_socket_ty = [`Datagram | `Platform of 'tag | `Shutdown | socket_ty]
type 'a datagram_socket = 'a r
  constraint 'a = [> [> `Generic] datagram_socket_ty]

type 'tag ty = [`Network | `Platform of 'tag]

type 'a t = 'a r
  constraint 'a = [> [> `Generic] ty]

(** {2 Socket options} *)

module Sockopt : sig
  (** An extensible type for socket options. Portable options can be defined
      here, while platform-specific options can be added by backends.

      @since 1.4 *)

  type _ t = ..

  val pp : 'a t Fmt.t
  val pp_binding : ('a t * 'a) Fmt.t

  type settings =
    | [] : settings
    | (::) : ('a t * 'a) * settings -> settings
  (** A heterogeneous list of socket options, each paired with the value to
      set it to. For example:

      {[ Eio.Net.Sockopt.[ (SO_REUSEADDR, true); (SO_SNDBUF, 4096) ] ]} *)

  (** {2 Common options} *)

  type _ t +=
    | SO_DEBUG : bool t         (** Enable socket debugging *)
    | SO_BROADCAST : bool t     (** Permit sending of broadcast messages *)
    | SO_REUSEADDR : bool t     (** Allow reuse of local addresses *)
    | SO_KEEPALIVE : bool t     (** Keep TCP connection alive *)
    | SO_DONTROUTE : bool t     (** Bypass routing tables *)
    | SO_OOBINLINE : bool t     (** Leave out-of-band data in line *)
    | TCP_NODELAY : bool t      (** Disable Nagle's algorithm *)
    | IPV6_ONLY : bool t        (** Restrict to IPv6 only *)
    | SO_REUSEPORT : bool t     (** Allow reuse of local port *)
    | SO_SNDBUF : int t         (** Send buffer size *)
    | SO_RCVBUF : int t         (** Receive buffer size *)
    | SO_RCVLOWAT : int t       (** Receive low water mark *)
    | SO_SNDLOWAT : int t       (** Send low water mark *)
    | SO_LINGER : int option t  (** Linger on close if data present *)
    | SO_RCVTIMEO : float t     (** Receive timeout *)
    | SO_SNDTIMEO : float t     (** Send timeout *)

    | TCP_KEEPIDLE : int t
    (** Time (in seconds) the connection needs to remain idle before TCP starts sending keepalive probes.
        Available on FreeBSD, Linux, macOS, Windows. *)

    | TCP_KEEPINTVL : int t
    (** Interval (in seconds) between individual keepalive probes.
        Available on FreeBSD, Linux, macOS, Windows. *)

    | TCP_KEEPCNT : int t
    (** Maximum number of keepalive probes TCP should send before dropping the connection.
        Available on FreeBSD, Linux, macOS, Windows. *)

    | TCP_CONGESTION : string t
    (** Set the TCP congestion control algorithm to be used (e.g., "cubic", "reno").
        Unprivileged processes are restricted to algorithms in tcp_allowed_congestion_control.
        Available on FreeBSD, Linux. *)

  (** {2 Linux-specific options} *)

  type _ t +=
    | TCP_CORK : bool t
    (** When enabled, partial frames are not sent out.
        Data is only sent when the option is disabled or the buffer becomes full. *)
    | TCP_USER_TIMEOUT : int t
    (** Maximum time (in milliseconds) that transmitted data may remain unacknowledged
        before TCP will forcibly close the connection. *)
    | TCP_MAXSEG : int t
    (** The maximum segment size for outgoing TCP packets. If set before connection
        establishment, it also changes the MSS value announced to the other end. *)
    | TCP_LINGER2 : int option t
    (** Lifetime of orphaned FIN_WAIT2 sockets, in seconds.

        On set, [None] or [Some 0] requests the system default; [Some n] with
        [n >= 0] keeps orphaned sockets in FIN_WAIT2 for [n] seconds;
        negative values raise {!Invalid_argument}.

        On get, [Some n] is the effective timeout and [None] means
        FIN_WAIT2 lingering has been disabled. *)
    | TCP_DEFER_ACCEPT : int t
    (** Allow a listener to be awakened only when data arrives on the socket.
        Value is the maximum time in seconds to wait for data. *)
    | TCP_SYNCNT : int t
    (** Set the number of SYN retransmits that TCP should send before aborting
        the attempt to connect. Cannot exceed 255. *)
    | TCP_WINDOW_CLAMP : int t
    (** Bound the size of the advertised window to this value.
        The kernel imposes a minimum size. *)
    | TCP_QUICKACK : bool t
    (** Enable quickack mode if set or disable if cleared. In quickack mode,
        acks are sent immediately rather than delayed. This flag is not permanent. *)
    | TCP_FASTOPEN : int t
    (** Enable Fast Open (RFC 7413) on the listener socket. The value specifies
        the maximum length of pending SYNs (similar to backlog in listen). *)
    | IP_FREEBIND : bool t
    (** Allow binding to an IP address that is nonlocal or does not (yet) exist.
        This permits listening on a socket without requiring the underlying
        network interface to be up. *)
    | IP_BIND_ADDRESS_NO_PORT : bool t
    (** Inform the kernel to not reserve an ephemeral port when using bind()
        with a port number of 0. The port will be chosen at connect() time. *)
    | IP_LOCAL_PORT_RANGE : (int * int) t
    (** Set the per-socket local port range as (lower_bound, upper_bound).
        Both bounds are inclusive and must be in range 0-65535.
        Use (0, 0) to reset to system defaults. *)
    | IP_TTL : int t
    (** Set or retrieve the time-to-live field used in every packet sent from this socket.
        Valid range is 1-255. *)
    | IP_MTU : int t
    (** Retrieve the current known path MTU of the current socket.
        Only valid for connected sockets. This is a read-only option. *)
    | IP_MTU_DISCOVER : [`Want | `Dont | `Do | `Probe] t
    (** Set or receive the Path MTU Discovery setting for a socket.
        - [`Want]: Use per-route settings (fragment if needed)
        - [`Dont]: Never do Path MTU Discovery
        - [`Do]: Always do Path MTU Discovery (reject large datagrams with EMSGSIZE)
        - [`Probe]: Set DF but ignore Path MTU (for diagnostic tools) *)

  (** {2 Registering printers (for backend use only)} *)

  type printer_fn = {
    get : 'a. 'a t -> (string * 'a Fmt.t) option
  } [@@unboxed]

  val register_printer : printer_fn -> unit
  (** [register_printer { get }] adds [get] to the list of printers.

      If called with an option it recognises, it should return the option's name
      and a printer for values of the appropriate type. If the option isn't known,
      return [None]. *)
end

val setsockopt : [> `Socket] r -> 'a Sockopt.t -> 'a -> unit
(** [setsockopt s opt v] sets socket option [opt] to value [v] on socket [s].

    @raise Eio.Io if the operating system rejects the option for this socket.

    @since 1.4 *)

val getsockopt : [> `Socket] r -> 'a Sockopt.t -> 'a
(** [getsockopt s opt] gets the value of socket option [opt] on socket [s].

    @raise Eio.Io if the operating system rejects the option for this socket.

    @since 1.4 *)

(** {2 Out-bound Connections} *)

val connect :
  ?bind_to:Sockaddr.stream ->
  ?options:Sockopt.settings ->
  sw:Switch.t -> [> 'tag ty] t -> Sockaddr.stream -> 'tag stream_socket_ty r
(** [connect ~sw t addr] is a new socket connected to remote address [addr].

    The new socket will be closed when [sw] finishes, unless closed manually first.

    Before connecting, each option in [options] is applied in list order and
    then the socket is bound to [bind_to] (if given).

    For example, to make an outbound connection from a fixed local address:

    {[
      Eio.Net.connect ~sw net addr
        ~bind_to:(`Tcp (local_ip, 0))
        ~options:Eio.Net.Sockopt.[
            (SO_REUSEADDR, true);
            (SO_KEEPALIVE, true);
          ]
    ]}

    @param options Socket options to set before binding and connecting.
    @param bind_to Local address to bind to before connecting. This is how
      a multi-homed host chooses the local address that an outbound
      connection uses. *)

val with_tcp_connect :
  ?timeout:Time.Timeout.t ->
  host:string ->
  service:string ->
  [> 'tag ty] r ->
  ('tag stream_socket_ty r -> 'b) ->
  'b
(** [with_tcp_connect ~host ~service t f] creates a tcp connection [conn] to [host] and [service] and executes 
    [f conn].

    [conn] is closed after [f] returns (if it isn't already closed by then).

    [host] is either an IP address or a domain name, eg. "www.example.org", "www.ocaml.org" or "127.0.0.1".

    [service] is an IANA recognized service name or port number, eg. "http", "ftp", "8080" etc.
    See https://www.iana.org/assignments/service-names-port-numbers/service-names-port-numbers.xhtml.

    Addresses are tried in the order they are returned by {!getaddrinfo}, until one succeeds.

    @param timeout Limits how long to wait for each connection attempt before moving on to the next.
                   By default there is no timeout (beyond what the underlying network does).

    @raise Connection_failure A connection couldn't be established for any of the addresses defined for [host]. *)

(** {2 Incoming Connections} *)

val listen :
  ?reuse_addr:bool -> ?reuse_port:bool -> backlog:int -> sw:Switch.t ->
  [> 'tag ty] r -> Sockaddr.stream -> 'tag listening_socket_ty r
(** [listen ~sw ~backlog t addr] is a new listening socket bound to local address [addr].

    The new socket will be closed when [sw] finishes, unless closed manually first.

    On platforms that support this, passing port [0] will bind to a random port.

    For (non-abstract) Unix domain sockets, the path will be removed afterwards.

    @param backlog The number of pending connections that can be queued up (see listen(2)).
    @param reuse_addr Set the {!Unix.SO_REUSEADDR} socket option.
                      For Unix paths, also remove any stale left-over socket.
    @param reuse_port Set the {!Unix.SO_REUSEPORT} socket option. *)

val accept :
  sw:Switch.t ->
  [> 'tag listening_socket_ty] r ->
  'tag stream_socket_ty r * Sockaddr.stream
(** [accept ~sw socket] waits until a new connection is ready on [socket] and returns it.

    The new socket will be closed automatically when [sw] finishes, if not closed earlier.
    If you want to handle multiple connections, consider using {!accept_fork} instead. *)

val accept_fork :
  sw:Switch.t ->
  [> 'tag listening_socket_ty] r ->
  on_error:(exn -> unit) ->
  [< 'tag stream_socket_ty] connection_handler ->
  unit
(** [accept_fork ~sw ~on_error socket fn] accepts a connection and handles it in a new fiber.

    After accepting a connection to [socket], it runs [fn flow client_addr] in a new fiber.

    [flow] will be closed when [fn] returns. The new fiber is attached to [sw].

    @param on_error Called if [connection_handler] raises an exception.
                    This is typically a good place to log the error and continue.
                    If the exception is an {!Eio.Io} error then the caller's address is added to it.

                    If you don't want to handle connection errors,
                    use [~on_error:raise] to cancel the caller's context.

                    [on_error] is not called for {!Cancel.Cancelled} exceptions,
                    which do not need to be reported. *)

val listening_addr : [> 'tag listening_socket_ty] r -> Sockaddr.stream

(** {2 Running Servers} *)

val run_server :
  ?max_connections:int ->
  ?additional_domains:(_ Domain_manager.t * int) ->
  ?stop:'a Promise.t ->
  on_error:(exn -> unit) ->
  [> 'tag listening_socket_ty ] r ->
  [< 'tag stream_socket_ty] connection_handler ->
  'a
(** [run_server ~on_error sock connection_handler] establishes a concurrent socket server [s].

    It accepts incoming client connections on socket [sock] and handles them with {!accept_fork}
    (see that for the description of [on_error] and [connection_handler]).

    {b Running a Parallel Server}

    By default [s] runs on a {e single} OCaml {!module:Domain}. However, if [additional_domains:(domain_mgr, domains)]
    parameter is given, then [s] will spawn [domains] additional domains and run accept loops in those too.
    In such cases you must ensure that [connection_handler] only accesses thread-safe values.
    Note that having more than {!Domain.recommended_domain_count} domains in total is likely to result in bad performance.

    For services that are bottlenecked on CPU rather than IO,
    you can run a single accept loop and have the handler submit CPU-intensive jobs to an {!module:Executor_pool}.

    @param max_connections The maximum number of concurrent connections accepted by [s] at any time.
                           The default is [Int.max_int].
    @param stop Resolving this promise causes [s] to stop accepting new connections.
                [run_server] will wait for all existing connections to finish and then return.
                This is useful to upgrade a server without clients noticing.
                To stop immediately, cancelling all connections, just cancel [s]'s fiber instead.
    @param on_error Connection error handler (see {!accept_fork}).
    @raise Invalid_argument if [max_connections <= 0].
                            if [additional_domains = (domain_mgr, domains)] is used and [domains < 0]. *)

(** {2 Datagram Sockets} *)

val datagram_socket :
     ?reuse_addr:bool
  -> ?reuse_port:bool
  -> sw:Switch.t
  -> [> 'tag ty] r
  -> [< Sockaddr.datagram | `UdpV4 | `UdpV6]
  -> 'tag datagram_socket_ty r
  (** [datagram_socket ~sw t addr] creates a new datagram socket bound to [addr]. The new 
      socket will be closed when [sw] finishes. 

      [`UdpV4] and [`UdpV6] represents IPv4 and IPv6
      datagram client sockets where the OS assigns the next available socket address and port
      automatically. [`Udp ..] can be used to create both listening server socket and client 
      socket.

      @param reuse_addr Set the {!Unix.SO_REUSEADDR} socket option.
      @param reuse_port Set the {!Unix.SO_REUSEPORT} socket option. *)

val send : _ datagram_socket -> ?dst:Sockaddr.datagram -> Cstruct.t list -> unit
(** [send sock buf] sends the data in [buf] using the the datagram socket [sock].

    @param dst If [sock] isn't connected, this provides the destination. *)

val recv : _ datagram_socket -> Cstruct.t -> Sockaddr.datagram * int
(** [recv sock buf] receives data from the socket [sock] putting it in [buf]. The number of bytes received is 
    returned along with the sender address and port. If the [buf] is too small then excess bytes may be discarded
    depending on the type of the socket the message is received from. *)

(** {2 DNS/getaddrinfo queries}

    Note that unlike {!Unix.getaddrinfo}, Eio's [getaddrinfo] family
    of functions raise an exception (see {!Address_lookup_failed}) with an error
    code instead of returning an empty list. *)

val getaddrinfo: ?service:string -> _ t -> string -> Sockaddr.t list
(** [getaddrinfo ?service t node] returns a list of IP addresses for [node]. [node] is either a domain name or
    an IP address.

    @param service is a human friendly textual name for internet services assigned by IANA., eg.
    'http', 'https', 'ftp', etc.

    For a more thorough treatment, see {{:https://man7.org/linux/man-pages/man3/getaddrinfo.3.html} getaddrinfo}.

    Never returns an empty list; it will raise [Eio.Io (Eio.Net.E Address_lookup_failed _, _)] instead. *)

val getaddrinfo_stream: ?service:string -> _ t -> string -> Sockaddr.stream list
(** [getaddrinfo_stream] is like {!getaddrinfo}, but filters out non-stream protocols.

    Never returns an empty list; it will raise [Eio.Io (Eio.Net.E Address_lookup_failed _, _)] instead. *)

val getaddrinfo_datagram: ?service:string -> _ t -> string -> Sockaddr.datagram list
(** [getaddrinfo_datagram] is like {!getaddrinfo}, but filters out non-datagram protocols.

    Never returns an empty list; it will raise [Eio.Io (Eio.Net.E Address_lookup_failed _, _)] instead. *)

val getnameinfo : _ t -> Sockaddr.t -> (string * string)
(** [getnameinfo t sockaddr] is [(hostname, service)] corresponding to [sockaddr]. [hostname] is the
    registered domain name represented by [sockaddr]. [service] is the IANA specified textual name of the
    port specified in [sockaddr], e.g. 'ftp', 'http', 'https', etc. *)

(** {2 Closing} *)

val close : [> `Close] r -> unit
(** Alias of {!Resource.close}. *)

(** {2 Provider Interface} *)

module Pi : sig
  module type SOCKET = sig
    type t
    val setsockopt : t -> 'a Sockopt.t -> 'a -> unit
    val getsockopt : t -> 'a Sockopt.t -> 'a
  end

  module type STREAM_SOCKET = sig
    type tag
    include Flow.Pi.SHUTDOWN
    include Flow.Pi.SOURCE with type t := t
    include Flow.Pi.SINK with type t := t
    include SOCKET with type t := t
    val close : t -> unit
  end

  val stream_socket :
    (module STREAM_SOCKET with type t = 't and type tag = 'tag) ->
    ('t, 'tag stream_socket_ty) Resource.handler

  module type DATAGRAM_SOCKET = sig
    type tag
    include Flow.Pi.SHUTDOWN
    include SOCKET with type t := t
    val send : t -> ?dst:Sockaddr.datagram -> Cstruct.t list -> unit
    val recv : t -> Cstruct.t -> Sockaddr.datagram * int
    val close : t -> unit
  end

  val datagram_socket :
    (module DATAGRAM_SOCKET with type t = 't and type tag = 'tag) ->
    ('t, 'tag datagram_socket_ty) Resource.handler

  module type LISTENING_SOCKET = sig
    type t
    type tag
    include SOCKET with type t := t
    val accept : t -> sw:Switch.t -> tag stream_socket_ty r * Sockaddr.stream
    val close : t -> unit
    val listening_addr : t -> Sockaddr.stream
  end

  val listening_socket :
    (module LISTENING_SOCKET with type t = 't and type tag = 'tag) ->
    ('t, 'tag listening_socket_ty) Resource.handler

  module type NETWORK = sig
    type t
    type tag

    val listen :
      t -> reuse_addr:bool -> reuse_port:bool -> backlog:int -> sw:Switch.t ->
      Sockaddr.stream -> tag listening_socket_ty r

    val connect :
      t -> bind_to:Sockaddr.stream option -> options:Sockopt.settings ->
      sw:Switch.t -> Sockaddr.stream -> tag stream_socket_ty r

    val datagram_socket :
      t
      -> reuse_addr:bool
      -> reuse_port:bool
      -> sw:Switch.t
      -> [Sockaddr.datagram | `UdpV4 | `UdpV6]
      -> tag datagram_socket_ty r

    val getaddrinfo : t -> service:string -> string -> Sockaddr.t list
    val getnameinfo : t -> Sockaddr.t -> (string * string)
  end

  val network :
    (module NETWORK with type t = 't and type tag = 'tag) ->
    ('t, 'tag ty) Resource.handler
end
