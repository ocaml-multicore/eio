open Fibreslib

module Generic = struct
  type 'a ty = ..

  class type t = object
    method probe : 'a. 'a ty -> 'a option
  end

  let probe (t : #t) ty = t#probe ty
end

module Flow = struct
  class type close = object
    method close : unit
  end

  let close (t : #close) = t#close

  class virtual read = object
    method virtual read_into : ?sw:Switch.t -> Cstruct.t -> int
  end

  let read_into ?sw (t : #read) buf =
    let got = t#read_into ?sw buf in
    assert (got > 0);
    got

  class virtual source = object (_ : #Generic.t)
    method probe _ = None
    inherit read
  end

  let string_source s : source =
    object
      inherit source

      val mutable data = Cstruct.of_string s

      method read_into ?sw buf =
        Option.iter Switch.check sw;
        match Cstruct.length data with
        | 0 -> raise End_of_file
        | remaining ->
          let len = min remaining (Cstruct.length buf) in
          Cstruct.blit data 0 buf 0 len;
          data <- Cstruct.shift data len;
          len
    end

  let cstruct_source data : source =
    object
      val mutable data = data

      inherit source

      method read_into ?sw dst =
        Option.iter Switch.check sw;
        let avail, src = Cstruct.fillv ~dst ~src:data in
        if avail = 0 then raise End_of_file;
        data <- src;
        avail
    end

  class virtual write = object
    method virtual write : 'a. ?sw:Switch.t -> (#source as 'a) -> unit
  end

  let copy ?sw (src : #source) (dst : #write) = dst#write ?sw src

  let copy_string ?sw s = copy ?sw (string_source s)

  class virtual sink = object (_ : #Generic.t)
    method probe _ = None
    inherit write
  end

  let buffer_sink b =
    object
      inherit sink

      method write ?sw src =
        let buf = Cstruct.create 4096 in
        try
          while true do
            let got = src#read_into ?sw buf in
            Buffer.add_string b (Cstruct.to_string ~len:got buf)
          done
        with End_of_file -> ()
    end

  class virtual two_way = object (_ : #Generic.t)
    method probe _ = None
    inherit read
    inherit write

    method virtual shutdown : Unix.shutdown_command -> unit
  end

  let shutdown (t : #two_way) = t#shutdown
end

module Network = struct
  module Sockaddr = struct
    type t = Unix.sockaddr

    let pp f = function
      | Unix.ADDR_UNIX path ->
        Format.fprintf f "unix:%s" path
      | Unix.ADDR_INET (addr, port) ->
        Format.fprintf f "inet:%s:%d" (Unix.string_of_inet_addr addr) port
  end

  module Listening_socket = struct
    class virtual t = object
      method virtual close : unit
      method virtual listen : int -> unit
      method virtual accept_sub :
        sw:Switch.t ->
        on_error:(exn -> unit) ->
        (sw:Switch.t -> <Flow.two_way; Flow.close> -> Sockaddr.t -> unit) ->
        unit
    end

    let listen (t : #t) = t#listen
    let accept_sub ~sw (t : #t) = t#accept_sub ~sw
  end

  class virtual t = object
    method virtual bind : reuse_addr:bool -> sw:Switch.t -> Sockaddr.t -> Listening_socket.t
    method virtual connect : sw:Switch.t -> Sockaddr.t -> <Flow.two_way; Flow.close>
  end

  let bind ?(reuse_addr=false) ~sw (t:#t) = t#bind ~reuse_addr ~sw
  let connect ~sw (t:#t) = t#connect ~sw
end

module Stdenv = struct
  type t = <
    stdin  : Flow.source;
    stdout : Flow.sink;
    stderr : Flow.sink;
    network : Network.t;
  >

  let stdin  (t : <stdin  : #Flow.source; ..>) = t#stdin
  let stdout (t : <stdout : #Flow.sink;   ..>) = t#stdout
  let stderr (t : <stderr : #Flow.sink;   ..>) = t#stderr

  let network (t : <network : #Network.t; ..>) = t#network
end
