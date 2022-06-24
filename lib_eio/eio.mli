(** Effects based parallel IO for OCaml.

    Eio provides support for concurrency (juggling many tasks) and
    parallelism (using multiple CPU cores for performance).

    It provides facilities for creating and coordinating fibers (light-weight
    threads) and domains (for parallel processing), as well as interfaces for
    interacting with resources provided by the operating system.

    These features must be used within an {e event loop},
    provided by an Eio {e backend}.
    Applications can use {!Eio_main.run} to run a suitable loop.

    See {{:https://github.com/ocaml-multicore/eio}} for a tutorial. *)

(** {1 Concurrency primitives} *)

(** Grouping fibers and other resources so they can be turned off together. *)
module Switch = Eio__core.Switch

(** A promise is a placeholder for result that will arrive in the future. *)
module Promise = Eio__core.Promise

(** A fiber is a light-weight thread. *)
module Fiber = Eio__core.Fiber

(**/**)
module Fibre = Fiber [@@deprecated "Now spelt Fiber"]
(**/**)

(** A counting semaphore. *)
module Semaphore : sig
  (** The API is based on OCaml's [Semaphore.Counting].

      The difference is that when waiting for the semaphore this will switch to the next runnable fiber,
      whereas the stdlib one will block the whole domain.

      Semaphores are thread-safe and so can be shared between domains and used
      to synchronise between them. *)

  type t
  (** The type of counting semaphores. *)

  val make : int -> t
  (** [make n] returns a new counting semaphore, with initial value [n].
      The initial value [n] must be nonnegative.
      @raise Invalid_argument if [n < 0] *)

  val release : t -> unit
  (** [release t] increments the value of semaphore [t].
      If other fibers are waiting on [t], the one that has been waiting the longest is resumed.
      @raise Sys_error if the value of the semaphore would overflow [max_int] *)

  val acquire : t -> unit
  (** [acquire t] blocks the calling fiber until the value of semaphore [t]
      is not zero, then atomically decrements the value of [t] and returns. *)

  val get_value : t -> int
  (** [get_value t] returns the current value of semaphore [t]. *)
end

(** Mutual exclusion. *)
module Mutex : sig 
  (** A mutex can be used to ensure that only one piece of code can access a shared resource at one time.

      Unlike {!Stdlib.Mutex}, which blocks the whole domain while waiting to take the mutex,
      this module allows other Eio fibers to run while waiting.
      You should use this module if your critical section may perform blocking operations,
      while [Stdlib.Mutex] may be more efficient if the lock is held only briefly and
      the critial section does not switch fibers.

      Note that mutexes are often unnecessary for code running in a single domain, as
      the scheduler will only switch to another fiber if you perform an operation that
      can block. *)
  
  type t
  (** The type for a concurrency-friendly mutex. *)

  exception Poisoned of exn
  (** Raised if you attempt to use a mutex that has been disabled. *)
  
  val create : unit -> t
  (** [create ()] creates an initially unlocked mutex. *)

  val use_rw : protect:bool -> t -> (unit -> 'a) -> 'a
  (** [use_rw ~protect t fn] waits for the mutex to be free and then executes [fn ()] while holding the mutex locked.
      [fn] may mutate the resource protected by the mutex,
      but must ensure the resource is in a consistent state before returning.
      If [fn] raises an exception, the mutex is disabled and cannot be used again.
      @param protect If [true], uses {!Cancel.protect} to prevent the critical section from being cancelled.
                     Cancellation is not prevented while waiting to take the lock. *)

  val use_ro : t -> (unit -> 'a) -> 'a
  (** [use_ro t fn] is like {!use_rw ~protect:false},
      but if [fn] raises an exception it unlocks the mutex instead of disabling it.
      Use this if you only need read-only access to the mutex's resource and so
      know that it will be in a consistent state even if an exception is raised. *)

  (** {2 Low-level API}

      Care must be taken when locking a mutex manually. It is easy to forget to unlock it in some cases,
      which will result in deadlock the next time a fiber tries to use it.
      In particular, you need to consider:

      - What happens if your critical section raises an exception.
      - What happens if your fiber is cancelled while in its critical section.
   *)
  
  val lock : t -> unit
  (** Lock the given mutex. Only one fiber can have the mutex locked at any time.
      A fiber that attempts to lock a mutex already locked by another fiber
      will suspend until the other fiber unlocks the mutex.
      If no other fiber has the lock, this returns immediately without switching fibers. *)
  
  val unlock : t -> unit
  (** [unlock t] unlocks the mutex.
      @raises Sys_error if the mutex is unlocked. *)

  val try_lock : t -> bool
  (** Same as {!lock}, but does not suspend the calling thread if the mutex is already locked:
      just return [false] immediately in that case. If the mutex is unlocked, lock it and return [true]. *)
end

(** A stream/queue. *)
module Stream : sig
  (** Reading from an empty queue will wait until an item is available.
      Writing to a full queue will wait until there is space.

      Example:
      {[
        let t = Stream.create 100 in
        Stream.add t 1;
        Stream.add t 2;
        assert (Stream.take t = 1);
        assert (Stream.take t = 2)
      ]}

      Streams are thread-safe and so can be shared between domains and used
      to communicate between them. *)

  type 'a t
  (** A queue of items of type ['a]. *)

  val create : int -> 'a t
  (** [create capacity] is a new stream which can hold up to [capacity] items without blocking writers.

      - If [capacity = 0] then writes block until a reader is ready.
      - If [capacity = 1] then this acts as a "mailbox".
      - If [capacity = max_int] then the stream is effectively unbounded. *)

  val add : 'a t -> 'a -> unit
  (** [add t item] adds [item] to [t].

      If this would take [t] over capacity, it blocks until there is space. *)

  val take : 'a t -> 'a
  (** [take t] takes the next item from the head of [t].

      If no items are available, it waits until one becomes available. *)

  val take_nonblocking : 'a t -> 'a option
  (** [take_nonblocking t] is like [Some (take t)] except that
      it returns [None] if the stream is empty rather than waiting.

      Note that if another domain may add to the stream then a [None]
      result may already be out-of-date by the time this returns. *)

  val length : 'a t -> int
  (** [length t] returns the number of items currently in [t]. *)

  val is_empty : 'a t -> bool
  (** [is_empty t] is [length t = 0]. *)
end

(** Cancelling fibers. *)
module Cancel = Eio__core.Cancel

(** Commonly used standard features. This module is intended to be [open]ed. *)
module Std : sig
  module Promise = Promise
  module Fiber = Fiber
  (**/**)
  module Fibre = Fiber [@@deprecated "Now spelt Fiber"]
  (**/**)
  module Switch = Switch

  val traceln :
    ?__POS__:string * int * int * int ->
    ('a, Format.formatter, unit, unit) format4 -> 'a
    (** Same as {!Eio.traceln}. *)
end

(** {1 Cross-platform OS API}

    The general pattern here is that each type of resource has a set of functions for using it,
    plus an object type to allow defining your own implementations.
    To use the resources, it is recommended that you use the functions rather than calling
    methods directly. Using the functions results in better error messages from the compiler,
    and may provide extra features or sanity checks.

    The system resources are available from the {!Stdenv.t} provided by your event loop
    (e.g. {!Lwt_main.run}). *)

(** A base class for objects that can be queried at runtime for extra features. *)
module Generic : sig
  type 'a ty = ..
  (** An ['a ty] is a query for a feature of type ['a]. *)

  class type t = object
    method probe : 'a. 'a ty -> 'a option
  end

  val probe : #t -> 'a ty -> 'a option
  (** [probe t feature] checks whether [t] supports [feature].
      This is mostly for internal use.
      For example, {!Eio_unix.FD.peek} uses this to get the underlying Unix file descriptor from a flow. *)
end

(** Byte streams. *)
module Flow : sig
  (** Flows are used to represent byte streams, such as open files and network sockets.
      A {!source} provides a stream of bytes. A {!sink} consumes a stream.
      A {!two_way} can do both.

      To read structured data (e.g. a line at a time), wrap a source using {!Buf_read}. *)

  (** {2 Reading} *)

  type read_method = ..
  (** Sources can offer a list of ways to read them, in order of preference. *)

  class virtual source : object
    inherit Generic.t
    method read_methods : read_method list
    method virtual read_into : Cstruct.t -> int
  end

  val read : #source -> Cstruct.t -> int
  (** [read src buf] reads one or more bytes into [buf].

      It returns the number of bytes read (which may be less than the
      buffer size even if there is more data to be read).
      [read src] just makes a single call to [src#read_into]
      (and asserts that the result is in range).

      - Use {!read_exact} instead if you want to fill [buf] completely.
      - Use {!Buf_read.line} to read complete lines.
      - Use {!copy} to stream data directly from a source to a sink.

      [buf] must not be zero-length.

      @raise End_of_file if there is no more data to read *)

  val read_exact : #source -> Cstruct.t -> unit
  (** [read_exact src dst] keeps reading into [dst] until it is full.
      @raise End_of_file if the buffer could not be filled. *)

  val read_methods : #source -> read_method list
  (** [read_methods flow] is a list of extra ways of reading from [flow],
      with the preferred (most efficient) methods first.

      If no method is suitable, {!read} should be used as the fallback. *)

  val string_source : string -> source
  (** [string_source s] is a source that gives the bytes of [s]. *)

  val cstruct_source : Cstruct.t list -> source
  (** [cstruct_source cs] is a source that gives the bytes of [cs]. *)

  type read_method += Read_source_buffer of ((Cstruct.t list -> unit) -> unit)
  (** If a source offers [Read_source_buffer rsb] then the user can call [rsb fn]
      to borrow a view of the source's buffers.

      [rsb] will raise [End_of_file] if no more data will be produced.
      If no data is currently available, [rsb] will wait for some to become available before calling [fn].

      [fn] must not continue to use the buffers after it returns. *)

  (** {2 Writing} *)

  (** Consumer base class. *)
  class virtual sink : object
    inherit Generic.t
    method virtual copy : 'a. (#source as 'a) -> unit
  end

  val copy : #source -> #sink -> unit
  (** [copy src dst] copies data from [src] to [dst] until end-of-file. *)

  val copy_string : string -> #sink -> unit
  (** [copy_string s = copy (string_source s)] *)

  val buffer_sink : Buffer.t -> sink
  (** [buffer_sink b] is a sink that adds anything sent to it to [b]. *)

  (** {2 Bidirectional streams} *)

  type shutdown_command = [
    | `Receive  (** Indicate that no more reads will be done *)
    | `Send     (** Indicate that no more writes will be done *)
    | `All      (** Indicate that no more reads or writes will be done *)
  ]

  class virtual two_way : object
    inherit source
    inherit sink

    method virtual shutdown : shutdown_command -> unit
  end

  val shutdown : #two_way -> shutdown_command -> unit
  (** [shutdown t cmd] indicates that the caller has finished reading or writing [t]
      (depending on [cmd]).

      This is useful in some protocols to indicate that you have finished sending the request,
      and that the remote peer should now send the response. *)

  (** {2 Closing}

      Flows are usually attached to switches and closed automatically when the switch
      finished. However, it can be useful to close them sooner manually in some cases. *)

  class type close = object
    method close : unit
  end

  val close : #close -> unit
  (** [close t] marks the flow as closed. It can no longer be used after this. *)
end

(** Buffered input and parsing *)
module Buf_read : sig
  (** This module provides fairly efficient non-backtracking parsers.
      It is modelled on Angstrom's API, and you should use that if
      backtracking is needed.

      Example:
      {[
        let r = Buf_read.of_flow flow ~max_size:1_000_000 in
        Buf_read.line r
      ]}
  *)

  type t
  (** An input buffer. *)

  exception Buffer_limit_exceeded
  (** Raised if parsing an item would require enlarging the buffer beyond its configured limit. *)

  type 'a parser = t -> 'a
  (** An ['a parser] is a function that consumes and returns a value of type ['a].
      @raise Failure The flow can't be parsed as a value of type ['a].
      @raise End_of_file The flow ended without enough data to parse an ['a].
      @raise Buffer_limit_exceeded Parsing the value would exceed the configured size limit. *)

  val parse : ?initial_size:int -> max_size:int -> 'a parser -> #Flow.source -> ('a, [> `Msg of string]) result
  (** [parse p flow ~max_size] uses [p] to parse everything in [flow].

      It is a convenience function that does
      {[
        let buf = of_flow flow ~max_size in
        format_errors (p <* eof) buf
      ]}

      @param initial_size see {!of_flow}. *)

  val parse_exn : ?initial_size:int -> max_size:int -> 'a parser -> #Flow.source -> 'a
  (** [parse_exn] wraps {!parse}, but raises [Failure msg] if that returns [Error (`Msg msg)].

      Catching exceptions with [parse] and then raising them might seem pointless,
      but this has the effect of turning e.g. an [End_of_file] exception into a [Failure]
      with a more user-friendly message. *)

  val parse_string : 'a parser -> string -> ('a, [> `Msg of string]) result
  (** [parse_string p s] uses [p] to parse everything in [s].
      It is defined as [format_errors (p <* end_of_input) (of_string s)] *)

  val parse_string_exn : 'a parser -> string -> 'a
  (** [parse_string_exn] is like {!parse_string}, but handles errors like {!parse_exn}. *)

  val of_flow : ?initial_size:int -> max_size:int -> #Flow.source -> t
  (** [of_flow ~max_size flow] is a buffered reader backed by [flow].

      @param initial_size The initial amount of memory to allocate for the buffer.
      @param max_size The maximum size to which the buffer may grow.
                      This must be large enough to hold the largest single item
                      you want to parse (e.g. the longest line, if using
                      {!line}), plus any terminator needed to know the value is
                      complete (e.g. the newline character(s)). This is just to
                      prevent a run-away input from consuming all memory, and
                      you can usually just set it much larger than you expect
                      to need. *)

  val of_buffer : Cstruct.buffer -> t
  (** [of_buffer buf] is a reader that reads from [buf].
      [buf] is used directly, without being copied.
      [eof_seen (of_buffer buf) = true].
      This module will not modify [buf] itself, but it will expose it via {!peek}. *)

  val of_string : string -> t
  (** [of_string s] is a reader that reads from [s]. *)

  val as_flow : t -> Flow.source
  (** [as_flow t] is a buffered flow.

      Reading from it will return data from the buffer,
      only reading the underlying flow if the buffer is empty. *)

  (** {2 Reading data} *)

  val line : string parser
  (** [line] parses one line.

      Lines can be terminated by either LF or CRLF.
      The returned string does not include the terminator.

      If [End_of_file] is reached after seeing some data but before seeing a line
      terminator, the data seen is returned as the last line. *)

  val lines : string Seq.t parser
  (** [lines] returns a sequence that lazily reads the next line until the end of the input is reached.

      [lines = seq line ~stop:at_end_of_input] *)

  val char : char -> unit parser
  (** [char c] checks that the next byte is [c] and consumes it.
      @raise Failure if the next byte is not [c] *)

  val any_char : char parser
  (** [any_char] parses one character. *)

  val peek_char : char option parser
  (** [peek_char] returns [Some c] where [c] is the next character, but does not consume it.

      Returns [None] at the end of the input stream rather than raising [End_of_file]. *)

  val string : string -> unit parser
  (** [string s] checks that [s] is the next string in the stream and consumes it.

      @raise Failure if [s] is not a prefix of the stream. *)

  val take : int -> string parser
  (** [take n] takes exactly [n] bytes from the input. *)

  val take_all : string parser
  (** [take_all] takes all remaining data until end-of-file.

      Returns [""] if already at end-of-file.

      @raise Buffer_limit_exceeded if the remaining data exceeds or equals the buffer limit
             (it needs one extra byte to confirm it has reached end-of-file). *)

  val take_while : (char -> bool) -> string parser
  (** [take_while p] finds the first byte for which [p] is false
      and consumes and returns all bytes before that.

      If [p] is true for all remaining bytes, it returns everything until end-of-file.

      It will return the empty string if there are no matching characters
      (and therefore never raises [End_of_file]). *)

  val skip_while : (char -> bool) -> unit parser
  (** [skip_while p] skips zero or more bytes for which [p] is [true].

      [skip_while p t] does the same thing as [ignore (take_while p t)],
      except that it is not limited by the buffer size. *)

  val skip : int -> unit parser
  (** [skip n] discards the next [n] bytes.

      [skip n] = [map ignore (take n)],
      except that the number of skipped bytes may be larger than the buffer (it will not grow).

      Note: if [End_of_file] is raised, all bytes in the stream will have been consumed. *)

  val at_end_of_input : bool parser
  (** [at_end_of_input] returns [true] when at the end of the stream, or
      [false] if there is at least one more byte to be read. *)

  val end_of_input : unit parser
  (** [end_of_input] checks that there are no further bytes in the stream.
      @raise Failure if there are further bytes *)

  (** {2 Combinators} *)

  val seq : ?stop:bool parser -> 'a parser -> 'a Seq.t parser
  (** [seq p] is a sequence that uses [p] to get the next item.

      A sequence node can only be used while the stream is at
      the expected position, and will raise [Invalid_argument]
      if any bytes have been consumed in the meantime. This
      also means that each node can only be used once; use
      {!Seq.memoize} to make the sequence persistent.

      It is not necessary to consume all the elements of the
      sequence.

      @param stop This is used before parsing each item.
                  The sequence ends if this returns [true].
                  The default is {!at_end_of_input}. *)

  val pair : 'a parser -> 'b parser -> ('a * 'b) parser
  (** [pair a b] is a parser that first uses [a] to parse a value [x],
      then uses [b] to parse a value [y], then returns [(x, y)].

      Note that this module does not support backtracking, so if [b] fails
      then the bytes consumed by [a] are lost. *)

  val return : 'a -> 'a parser
  (** [return x] is a parser that consumes nothing and always returns [x].
      [return] is just [Fun.const]. *)

  val map : ('a -> 'b) -> ('a parser -> 'b parser)
  (** [map f a] is a parser that parses the stream with [a] to get [v],
      and then returns [f v]. *)

  val bind : 'a parser -> ('a -> 'b parser) -> 'b parser
  (** [bind a f] is a parser that first uses [a] to parse a value [v],
      then uses [f v] to select the next parser, and then uses that. *)

  val format_errors : 'a parser -> ('a, [> `Msg of string]) result parser
  (** [format_errors p] catches [Failure], [End_of_file] and
      [Buffer_limit_exceeded] exceptions and returns them as a formatted error message. *)

  (** Convenient syntax for some of the combinators. *)
  module Syntax : sig
    val ( let+ ) : 'a parser -> ('a -> 'b) -> 'b parser
    (** Syntax for {!map}. *)

    val ( let* ) : 'a parser -> ('a -> 'b parser) -> 'b parser
    (** Syntax for {!bind} *)

    val ( and+ ) : 'a parser -> 'b parser -> ('a * 'b) parser
    (** Syntax for {!pair} *)

    val ( and* ) : 'a parser -> 'b parser -> ('a * 'b) parser
    (** Syntax for {!pair} (same as [and+]). *)

    val ( <*> ) : 'a parser -> 'b parser -> ('a * 'b) parser
    (** [a <*> b] is [pair a b]. *)

    val ( <* ) : 'a parser -> 'b parser -> 'a parser
    (** [a <* b] is [map fst (pair a b)].
        It parses two things and keeps only the first. *)

    val ( *> ) : 'a parser -> 'b parser -> 'b parser
    (** [a *> b] is [map snd (pair a b)].
        It parses two things and keeps only the second. *)
  end

  (** {2 Low-level API} *)

  val buffered_bytes : t -> int
  (** [buffered_bytes t] is the number of bytes that can be read without
      reading from the underlying flow. *)

  val peek : t -> Cstruct.t
  (** [peek t] returns a view onto the active part of [t]'s internal buffer.

      Performing any operation that might add to the buffer may invalidate this,
      so it should be used immediately and then forgotten.

      [Cstruct.length (peek t) = buffered_bytes t]. *)

  val ensure : t -> int -> unit
  (** [ensure t n] ensures that the buffer contains at least [n] bytes of data.

      If not, it reads from the flow until there is.

      [buffered_bytes (ensure t n) >= n].

      @raise End_of_file if the flow ended before [n] bytes were available
      @raise Buffer_limit_exceeded if [n] exceeds the buffer's maximum size *)

  val consume : t -> int -> unit
  (** [consume t n] discards the first [n] bytes from [t]'s buffer.

      Use this after {!peek} to mark some bytes as consumed.

      [buffered_bytes t' = buffered_bytes t - n]

      Note: unlike {!skip}, this will not read data from the underlying flow. *)

  val consumed_bytes : t -> int
  (** [consumed_bytes t] is the total number of bytes consumed.

      i.e. it is the offset into the stream of the next byte to be parsed. *)

  val eof_seen : t -> bool
  (** [eof_seen t] indicates whether we've received [End_of_file] from the underlying flow.

      If so, there will never be any further data beyond what [peek] already returns.

      Note that this returns [false] if we're at the end of the stream but don't know it yet.
      Use {!at_end_of_input} to be sure. *)
end

(** Networking. *)
module Net : sig
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
      If you want to handle multiple connections, consider using {!accept_sub} instead. *)

  val accept_sub :
    sw:Switch.t ->
    #listening_socket ->
    on_error:(exn -> unit) ->
    (sw:Switch.t -> stream_socket -> Sockaddr.stream -> unit) ->
    unit
  (** [accept socket fn] accepts a connection and handles it in a new fiber.

      After accepting a connection to [socket], it runs [fn ~sw flow client_addr] in a new fiber,
      using {!Fiber.fork_on_accept}.

      [flow] will be closed automatically when the sub-switch is finished, if not already closed by then. *)

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
end

(** Parallel computation across multiple CPU cores. *)
module Domain_manager : sig
  class virtual t : object
    method virtual run_raw : 'a. (unit -> 'a) -> 'a

    method virtual run : 'a. (unit -> 'a) -> 'a
    (** Note: cancellation is handled by the {!run} wrapper function, not the object. *)
  end

  val run : #t -> (unit -> 'a) -> 'a
  (** [run t f] runs [f ()] in a newly-created domain and returns the result.

      Other fibers in the calling domain can run in parallel with the new domain.

      Warning: [f] must only access thread-safe values from the calling domain,
      but this is not enforced by the type system.

      If the calling fiber is cancelled, this is propagated to the spawned domain. *)

  val run_raw : #t -> (unit -> 'a) -> 'a
  (** [run_raw t f] is like {!run}, but does not run an event loop in the new domain,
      and so cannot perform IO, fork fibers, etc. *)
end

(** Clocks, time, sleeping and timeouts. *)
module Time : sig
  class virtual clock : object
    method virtual now : float
    method virtual sleep_until : float -> unit
  end

  val now : #clock -> float
  (** [now t] is the current time according to [t]. *)

  val sleep_until : #clock -> float -> unit
  (** [sleep_until t time] waits until the given time is reached. *)

  val sleep : #clock -> float -> unit
  (** [sleep t d] waits for [d] seconds. *)

  val with_timeout : #clock -> float -> (unit -> ('a, 'e) result) -> ('a, [> `Timeout] as 'e) result
  (** [with_timeout clock d fn] runs [fn ()] but cancels it after [d] seconds. *)

  exception Timeout

  val with_timeout_exn : #clock -> float -> (unit -> 'a) -> 'a
  (** [with_timeout_exn clock d fn] runs [fn ()] but cancels it after [d] seconds,
      raising exception [Timeout]. *)
end

(** Tranditional Unix permissions. *)
module Unix_perm : sig
  type t = int
  (** This is the same as {!Unix.file_perm}, but avoids a dependency on [Unix]. *)
end

(** File-system access. *)
module Dir : sig
  (** A [Dir.t] represents access to a directory and contents, recursively.

      {!Stdenv.fs} provides access to the whole file-system.

      Example:

      {[
        Eio.Dir.load fs "/etc/passwd"
      ]}
  *)

  type path = string

  exception Already_exists of path * exn
  exception Not_found of path * exn
  exception Permission_denied of path * exn

  class virtual rw : object
    inherit Generic.t
    inherit Flow.source
    inherit Flow.sink
  end

  (** When to create a new file. *)
  type create = [
    | `Never                            (** fail if the named file doesn't exist *)
    | `If_missing of Unix_perm.t        (** create if file doesn't already exist *)
    | `Or_truncate of Unix_perm.t       (** any existing file is truncated to zero length *)
    | `Exclusive of Unix_perm.t         (** always create; fail if the file already exists *)
  ]
  (** If a new file is created, the given permissions are used for it. *)

  class virtual t : object
    method virtual open_in : sw:Switch.t -> path -> <Flow.source; Flow.close>
    method virtual open_out :
      sw:Switch.t ->
      append:bool ->
      create:create ->
      path -> <rw; Flow.close>
    method virtual mkdir : perm:Unix_perm.t -> path -> unit
    method virtual open_dir : sw:Switch.t -> path -> t_with_close
    method virtual read_dir : path -> path list
  end
  and virtual t_with_close : object
    inherit t
    method virtual close : unit
  end

  (** {1 Reading files} *)

  val load : #t -> path -> string
  (** [load t path] returns the contents of the given file.

      This is a convenience wrapper around {!with_open_in}. *)

  val open_in : sw:Switch.t -> #t -> path -> <Flow.source; Flow.close>
  (** [open_in ~sw t path] opens [t/path] for reading.

      Note: files are always opened in binary mode. *)

  val with_open_in : #t -> path -> (<Flow.source; Flow.close> -> 'a) -> 'a
  (** [with_open_in] is like [open_in], but calls [fn flow] with the new flow and closes
      it automatically when [fn] returns (if it hasn't already been closed by then). *)

  val with_lines : #t -> path -> (string Seq.t -> 'a) -> 'a
  (** [with_lines t path fn] is a convenience function for streaming the lines of the file.

      It uses {!Buf_read.lines}. *)

  (** {1 Writing files} *)

  val save : ?append:bool -> create:create -> #t -> path -> string -> unit
  (** [save t path data ~create] writes [data] to [path].

      This is a convenience wrapper around {!with_open_out}. *)

  val open_out :
    sw:Switch.t ->
    ?append:bool ->
    create:create ->
    #t -> path -> <rw; Flow.close>
  (** [open_out ~sw t path] opens [t/path] for reading and writing.

      Note: files are always opened in binary mode.
      @param append Open for appending: always write at end of file.
      @param create Controls whether to create the file, and what permissions to give it if so. *)

  val with_open_out :
    ?append:bool ->
    create:create ->
    #t -> path -> (<rw; Flow.close> -> 'a) -> 'a
  (** [with_open_out] is like [open_out], but calls [fn flow] with the new flow and closes
      it automatically when [fn] returns (if it hasn't already been closed by then). *)

  (** {1 Directories} *)

  val mkdir : #t -> perm:Unix_perm.t -> path -> unit
  (** [mkdir t ~perm path] creates a new directory [t/path] with permissions [perm]. *)

  val open_dir : sw:Switch.t -> #t -> path -> <t; Flow.close>
  (** [open_dir ~sw t path] opens [t/path].

      This can be passed to functions to grant access only to the subtree [t/path]. *)

  val with_open_dir : #t -> path -> (<t; Flow.close> -> 'a) -> 'a
  (** [with_open_dir] is like [open_dir], but calls [fn dir] with the new directory and closes
      it automatically when [fn] returns (if it hasn't already been closed by then). *)

  val read_dir : #t -> path -> string list
  (** [read_dir t path] reads directory entries for [t/path]. The entries are sorted using {! String.compare}.*)
end

(** The standard environment of a process. *)
module Stdenv : sig
  (** All access to the outside world comes from running the event loop,
      which provides a {!t}.

      Example:
      {[
        let () =
          Eio_main.run @@ fun env ->
          Eio.Dir.with_open_dir env#fs "/srv/www" @@ fun www ->
          serve_files www
            ~net:env#net
      ]}
  *)

  type t = <
    stdin  : Flow.source;
    stdout : Flow.sink;
    stderr : Flow.sink;
    net : Net.t;
    domain_mgr : Domain_manager.t;
    clock : Time.clock;
    fs : Dir.t;
    cwd : Dir.t;
    secure_random : Flow.source;
  >

  (** {1 Standard streams}

      To use these, see {!Flow}. *)

  val stdin  : <stdin  : #Flow.source as 'a; ..> -> 'a
  val stdout : <stdout : #Flow.sink   as 'a; ..> -> 'a
  val stderr : <stderr : #Flow.sink   as 'a; ..> -> 'a

  (** {1 File-system access}

      To use these, see {!Dir}. *)

  val cwd : <cwd : #Dir.t as 'a; ..> -> 'a
  (** [cwd t] is the current working directory of the process (this may change
      over time if the process does a "chdir" operation, which is not recommended). *)

  val fs : <fs : #Dir.t as 'a; ..> -> 'a
  (** [fs t] is the process's full access to the filesystem.

      Paths can be absolute or relative (to the current working directory).
      Using relative paths with this is similar to using them with {!cwd},
      except that this will follow ".." and symlinks to other parts of the filesystem.

      [fs] is useful for handling paths passed in by the user. *)

  (** {1 Network}

      To use this, see {!Net}.
  *)

  val net : <net : #Net.t as 'a; ..> -> 'a
  (** [net t] gives access to the process's network namespace. *)

  (** {1 Domains (using multiple CPU cores)}

      To use this, see {!Domain_manager}.
  *)

  val domain_mgr : <domain_mgr : #Domain_manager.t as 'a; ..> -> 'a
  (** [domain_mgr t] allows running code on other cores. *)

  (** {1 Time}

      To use this, see {!Time}.
  *)

  val clock : <clock : #Time.clock as 'a; ..> -> 'a
  (** [clock t] is the system clock. *)

  (** {1 Randomness} *)

  val secure_random : <secure_random : #Flow.source as 'a; ..> -> 'a
  (** [secure_random t] is a source of random bytes suitable for cryptographic purposes. *)

end

(** {1 Errors and Debugging} *)

val traceln :
  ?__POS__:string * int * int * int ->
  ('a, Format.formatter, unit, unit) format4 -> 'a
(** [traceln fmt] outputs a debug message (typically to stderr).

    Trace messages are printed by default and do not require logging to be configured first.
    The message is printed with a newline, and is flushed automatically.
    [traceln] is intended for quick debugging rather than for production code.

    Unlike most Eio operations, [traceln] will never switch to another fiber;
    if the OS is not ready to accept the message then the whole domain waits.

    It is safe to call [traceln] from multiple domains at the same time.
    Each line will be written atomically.

    Examples:
    {[
      traceln "x = %d" x;
      traceln "x = %d" x ~__POS__;   (* With location information *)
    ]}
    @param __POS__ Display [__POS__] as the location of the [traceln] call. *)

(** Reporting multiple failures at once. *)
module Exn = Eio__core.Exn

(** {1 Provider API for OS schedulers} *)

(** API for use by the scheduler implementation. *)
module Private = Eio__core.Private
