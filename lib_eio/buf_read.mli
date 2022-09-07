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

val take_while1 : (char -> bool) -> string parser
(** [take_while1 p] is like [take_while]. However, the parser fails with "take_while1"
    if at least one character of input hasn't been consumed by the parser. *)

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
