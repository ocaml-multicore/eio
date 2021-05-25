(** A base class for objects that can be queried at runtime for extra features. *)
module Generic = struct
  type 'a ty = ..
  (** An ['a ty] is a query for a feature of type ['a]. *)

  class t = object
    method probe : 'a. 'a ty -> 'a option = fun _ -> None
  end

  let probe (t : #t) ty = t#probe ty
end

(** Producers of byte-streams. *)
module Source = struct
  class virtual t = object
    inherit Generic.t

    method virtual read_into : Cstruct.t -> int
    (** [read_into buf] reads one or more bytes into [buf].
        It returns the number of bytes written (which may be less than the
        buffer size even if there is more data to be read).
        [buf] must not be zero-length.
        @raise End_of_file if there is no more data to read *)
  end

  let read_into (t : #t) buf = t#read_into buf

  let of_string s =
    object
      inherit t

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
end

(** Consumers of byte-streams. *)
module Sink = struct
  class virtual t = object
    inherit Generic.t

    method virtual write : 'a. (#Source.t as 'a) -> unit
    (** [write src] writes data from [src] until end-of-file. *)
  end

  let write (t : #t) ~src = t#write src

  let of_buffer b =
    object
      inherit t

      method write src =
        let buf = Cstruct.create 4096 in
        try
          while true do
            let got = src#read_into buf in
            Buffer.add_string b (Cstruct.to_string ~len:got buf)
          done
        with End_of_file -> ()
    end
end

(** The standard environment of a process. *)
module Stdenv = struct
  type t = <
    stdin  : Source.t;
    stdout : Sink.t;
    stderr : Sink.t;
  >

  let stdin  (t : <stdin  : #Source.t; ..>) = t#stdin
  let stdout (t : <stdout : #Sink.t;   ..>) = t#stdout
  let stderr (t : <stderr : #Sink.t;   ..>) = t#stderr
end
