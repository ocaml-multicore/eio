(** A base class for objects that can be queried at runtime for extra features. *)
module Generic = struct
  type 'a ty = ..
  (** An ['a ty] is a query for a feature of type ['a]. *)

  class type t = object
    method probe : 'a. 'a ty -> 'a option
  end

  let probe (t : #t) ty = t#probe ty
end

(** Byte streams. *)
module Flow = struct
  class type close = object
    method close : unit
  end

  let close (t : #close) = t#close

  class virtual read = object
    method virtual read_into : Cstruct.t -> int
  end

  (** [read_into buf] reads one or more bytes into [buf].
      It returns the number of bytes written (which may be less than the
      buffer size even if there is more data to be read).
      [buf] must not be zero-length.
      @raise End_of_file if there is no more data to read *)
  let read_into (t : #read) buf =
    let got = t#read_into buf in
    assert (got > 0);
    got

  (** Producer base class. *)
  class virtual source = object (_ : #Generic.t)
    method probe _ = None
    inherit read
  end

  let string_source s : source =
    object
      inherit source

      val mutable data = Cstruct.of_string s

      method read_into buf =
        match Cstruct.length data with
        | 0 -> raise End_of_file
        | remaining ->
          let len = min remaining (Cstruct.length buf) in
          Cstruct.blit data 0 buf 0 len;
          data <- Cstruct.shift data len;
          len
    end

  class virtual write = object
    method virtual write : 'a. (#source as 'a) -> unit
  end

  (** [write src] writes data from [src] until end-of-file. *)
  let write (t : #write) ~src = t#write src

  (** Consumer base class. *)
  class virtual sink = object (_ : #Generic.t)
    method probe _ = None
    inherit write
  end

  let buffer_sink b =
    object
      inherit sink

      method write src =
        let buf = Cstruct.create 4096 in
        try
          while true do
            let got = src#read_into buf in
            Buffer.add_string b (Cstruct.to_string ~len:got buf)
          done
        with End_of_file -> ()
    end

  (** Bidirectional stream base class. *)
  class virtual two_way = object (_ : #Generic.t)
    method probe _ = None
    inherit read
    inherit write
  end
end

(** The standard environment of a process. *)
module Stdenv = struct
  type t = <
    stdin  : Flow.source;
    stdout : Flow.sink;
    stderr : Flow.sink;
  >

  let stdin  (t : <stdin  : #Flow.source; ..>) = t#stdin
  let stdout (t : <stdout : #Flow.sink;   ..>) = t#stdout
  let stderr (t : <stderr : #Flow.sink;   ..>) = t#stderr
end
