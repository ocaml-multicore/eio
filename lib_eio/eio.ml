open EffectHandlers

module Std = struct
  module Promise = Promise
  module Fibre = Fibre
  module Switch = Switch

  type _ eff += Trace : (?__POS__:(string * int * int * int) -> ('a, Format.formatter, unit, unit) format4 -> 'a) eff

  let traceln ?__POS__ fmt =
    perform Trace ?__POS__ fmt
end

module Semaphore = Semaphore
module Stream = Stream

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

  type read_method = ..
  type read_method += Read_source_buffer of (?sw:Switch.t -> (Cstruct.t list -> unit) -> unit)

  class type close = object
    method close : unit
  end

  let close (t : #close) = t#close

  class virtual read = object
    method virtual read_methods : read_method list
    method virtual read_into : ?sw:Switch.t -> Cstruct.t -> int
  end

  let read_into ?sw (t : #read) buf =
    let got = t#read_into ?sw buf in
    assert (got > 0);
    got

  let read_methods (t : #read) = t#read_methods

  class virtual source = object (_ : #Generic.t)
    method probe _ = None
    inherit read
  end

  let cstruct_source data : source =
    object (self)
      val mutable data = data

      inherit source

      method private read_source_buffer ?sw fn =
        Option.iter Switch.check sw;
        let rec aux () =
          match data with
          | [] -> raise End_of_file
          | x :: xs when Cstruct.length x = 0 -> data <- xs; aux ()
          | xs -> data <- []; fn xs
        in
        aux ()

      method read_methods =
        [ Read_source_buffer self#read_source_buffer ]

      method read_into ?sw dst =
        Option.iter Switch.check sw;
        let avail, src = Cstruct.fillv ~dst ~src:data in
        if avail = 0 then raise End_of_file;
        data <- src;
        avail
    end

  let string_source s = cstruct_source [Cstruct.of_string s]

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

module Net = struct
  exception Connection_reset of exn

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

  class virtual listening_socket = object
    method virtual close : unit
    method virtual accept_sub :
      sw:Switch.t ->
      on_error:(exn -> unit) ->
      (sw:Switch.t -> <Flow.two_way; Flow.close> -> Sockaddr.t -> unit) ->
      unit
  end

  let accept_sub ~sw (t : #listening_socket) = t#accept_sub ~sw

  class virtual t = object
    method virtual listen : reuse_addr:bool -> backlog:int -> sw:Switch.t -> Sockaddr.t -> listening_socket
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
    method virtual now : float
    method virtual sleep_until : ?sw:Switch.t -> float -> unit
  end

  let now (t : #clock) = t#now

  let sleep_until ?sw (t : #clock) time = t#sleep_until ?sw time

  let sleep ?sw t d = sleep_until ?sw t (now t +. d)
end

module Dir = struct
  type path = string

  exception Already_exists of path * exn
  exception Not_found of path * exn
  exception Permission_denied of path * exn

  class virtual rw = object (_ : #Generic.t)
    method probe _ = None
    inherit Flow.read
    inherit Flow.write
  end

  type create = [`Never | `If_missing of Unix.file_perm | `Or_truncate of Unix.file_perm | `Exclusive of Unix.file_perm]

  class virtual t = object
    method virtual open_in : sw:Switch.t -> path -> <Flow.source; Flow.close>
    method virtual open_out :
      sw:Switch.t ->
      append:bool ->
      create:create ->
      path -> <rw; Flow.close>
    method virtual mkdir : ?sw:Switch.t -> perm:Unix.file_perm -> path -> unit
    method virtual open_dir : sw:Switch.t -> path -> t_with_close
  end
  and virtual t_with_close = object
    (* This dummy class avoids an "Error: The type < .. > is not an object type" error from the compiler. *)
    inherit t
    method virtual close : unit
  end

  let open_in ~sw (t:#t) = t#open_in ~sw
  let open_out ~sw ?(append=false) ~create (t:#t) path = t#open_out ~sw ~append ~create path
  let open_dir ~sw (t:#t) = t#open_dir ~sw
  let mkdir ?sw (t:#t) = t#mkdir ?sw

  let with_open_in ?sw (t:#t) path fn =
    Switch.sub_opt sw @@ fun sw -> fn (open_in ~sw t path)

  let with_open_out ?sw ?append ~create (t:#t) path fn =
    Switch.sub_opt sw @@ fun sw -> fn (open_out ~sw ?append ~create t path)

  let with_open_dir ?sw (t:#t) path fn =
    Switch.sub_opt sw @@ fun sw -> fn (open_dir ~sw t path)
end

module Stdenv = struct
  type t = <
    stdin  : Flow.source;
    stdout : Flow.sink;
    stderr : Flow.sink;
    net : Net.t;
    domain_mgr : Domain_manager.t;
    clock : Time.clock;
    fs : Dir.t;
    cwd : Dir.t;
  >

  let stdin  (t : <stdin  : #Flow.source; ..>) = t#stdin
  let stdout (t : <stdout : #Flow.sink;   ..>) = t#stdout
  let stderr (t : <stderr : #Flow.sink;   ..>) = t#stderr
  let net (t : <net : #Net.t; ..>) = t#net
  let domain_mgr (t : <domain_mgr : #Domain_manager.t; ..>) = t#domain_mgr
  let clock (t : <clock : #Time.clock; ..>) = t#clock
  let fs (t : <fs : #Dir.t; ..>) = t#fs
  let cwd (t : <cwd : #Dir.t; ..>) = t#cwd
end

module Private = struct
  module Effects = struct
    type 'a enqueue = 'a Suspend.enqueue
    type _ eff += 
      | Suspend = Suspend.Suspend 
      | Fork = Fibre.Fork
      | Fork_ignore = Fibre.Fork_ignore
      | Trace = Std.Trace
  end
  module Switch = Switch
end
