module Std = struct
  module Promise = Promise
  module Fibre = Fibre
  module Switch = Switch

  let stderr_mutex = Mutex.create ()
  let traceln ?__POS__ fmt =
    fmt |> Format.kasprintf (fun msg ->
        Ctf.label msg;
        Mutex.lock stderr_mutex;
        Fun.protect ~finally:(fun () -> Mutex.unlock stderr_mutex)
          (fun () ->
             match __POS__ with
             | Some (file, lnum, _, _) -> Format.printf "%s:%d %s@." file lnum msg
             | None -> Format.printf "%s@." msg
          )
      )
end

module Semaphore = Semaphore

open Std

module Generic = struct
  type 'a ty = ..

  class type t = object
    method probe : 'a. 'a ty -> 'a option
  end

  let probe (t : #t) ty = t#probe ty
end

module Flow = struct
  type shutdown_command = [ `Receive | `Send | `All ]

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

    method virtual shutdown : shutdown_command -> unit
  end

  let shutdown (t : #two_way) = t#shutdown
end

module Network = struct
  module Sockaddr = struct
    type inet_addr = Unix.inet_addr

    type t = [
      | `Unix of string
      | `Tcp of inet_addr * int
    ]

    let pp f = function
      | `Unix path ->
        Format.fprintf f "unix:%s" path
      | `Tcp (addr, port) ->
        Format.fprintf f "tcp:%s:%d" (Unix.string_of_inet_addr addr) port
  end

  module Listening_socket = struct
    class virtual t = object
      method virtual close : unit
      method virtual accept_sub :
        sw:Switch.t ->
        on_error:(exn -> unit) ->
        (sw:Switch.t -> <Flow.two_way; Flow.close> -> Sockaddr.t -> unit) ->
        unit
    end

    let accept_sub ~sw (t : #t) = t#accept_sub ~sw
  end

  class virtual t = object
    method virtual listen : reuse_addr:bool -> backlog:int -> sw:Switch.t -> Sockaddr.t -> Listening_socket.t
    method virtual connect : sw:Switch.t -> Sockaddr.t -> <Flow.two_way; Flow.close>
  end

  let listen ?(reuse_addr=false) ~backlog ~sw (t:#t) = t#listen ~reuse_addr ~backlog ~sw
  let connect ~sw (t:#t) = t#connect ~sw
end

module Domain_manager = struct
  class virtual t = object
    method virtual run_compute_unsafe : 'a. (unit -> 'a) -> 'a
  end

  let run_compute_unsafe (t : #t) = t#run_compute_unsafe
end

module Time = struct
  class virtual clock = object
    method virtual sleep : ?sw:Switch.t -> float -> unit
  end

  let sleep ?sw (t : #clock) d = t#sleep ?sw d
end


module Stdenv = struct
  type t = <
    stdin  : Flow.source;
    stdout : Flow.sink;
    stderr : Flow.sink;
    network : Network.t;
    domain_mgr : Domain_manager.t;
    clock : Time.clock;
  >

  let stdin  (t : <stdin  : #Flow.source; ..>) = t#stdin
  let stdout (t : <stdout : #Flow.sink;   ..>) = t#stdout
  let stderr (t : <stderr : #Flow.sink;   ..>) = t#stderr
  let network (t : <network : #Network.t; ..>) = t#network
  let domain_mgr (t : <domain_mgr : #Domain_manager.t; ..>) = t#domain_mgr
  let clock (t : <clock : #Time.clock; ..>) = t#clock
end

module Private = struct
  module Effects = struct
    effect Await = Switch.Await
    effect Fork = Fibre.Fork
    effect Fork_ignore = Fibre.Fork_ignore
    effect Yield = Fibre.Yield
  end
  module Waiters = Waiters
  module Switch = Switch
end
