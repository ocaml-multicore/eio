open Std

type connection_failure =
  | Refused of Exn.Backend.t
  | No_matching_addresses
  | Timeout

type error =
  | Connection_reset of Exn.Backend.t
  | Connection_failure of connection_failure

type Exn.err += E of error

let err e = Exn.create (E e)

let () =
  Exn.register_pp (fun f -> function
      | E e ->
        Fmt.string f "Net ";
        begin match e with
          | Connection_reset e -> Fmt.pf f "Connection_reset %a" Exn.Backend.pp e
          | Connection_failure Refused e -> Fmt.pf f "Connection_failure Refused %a" Exn.Backend.pp e
          | Connection_failure Timeout -> Fmt.pf f "Connection_failure Timeout"
          | Connection_failure No_matching_addresses -> Fmt.pf f "Connection_failure No_matching_addresses"
        end;
        true
      | _ -> false
    )

module Ipaddr = struct
  type 'a t = string   (* = [Unix.inet_addr], but avoid a Unix dependency here *)

  module V4 = struct
    let any      = "\000\000\000\000"
    let loopback = "\127\000\000\001"

    let pp f t =
      Fmt.pf f "%d.%d.%d.%d"
        (Char.code t.[0])
        (Char.code t.[1])
        (Char.code t.[2])
        (Char.code t.[3])
  end

  module V6 = struct
    let any      = "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
    let loopback = "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001"

    let to_int16 t =
      let get i = Char.code (t.[i]) in
      let pair i = (get i lsl 8) lor (get (i + 1)) in
      List.init 8 (fun i -> pair (i * 2))

    (* [calc_elide elide zeros acc parts] finds the best place for the "::"
       when printing an IPv6 address.
       Returns [None, rev t] if there are no pairs of zeros, or
       [Some (-n), rev t'] where [n] is the length of the longest run of zeros
       and [t'] is [t] with all runs of zeroes replaced with [-len_run]. *)
    let calc_elide t =
      (* [elide] is the negative of the length of the best previous run of zeros seen.
         [zeros] is the current run.
         [acc] is the values seen so far, with runs of zeros replaced by a
         negative value giving the length of the run. *)
      let rec loop elide zeros acc = function
      | 0 :: xs -> loop elide (zeros - 1) acc xs
      | n :: xs when zeros = 0 -> loop elide 0 (n :: acc) xs
      | n :: xs -> loop (min elide zeros) 0 (n :: zeros :: acc) xs
      | [] ->
        let elide = min elide zeros in
        let parts = if zeros = 0 then acc else zeros :: acc in
        ((if elide < -1 then Some elide else None), List.rev parts)
          
      in
      loop 0 0 [] t

    let rec cons_zeros l x =
      if x >= 0 then l else cons_zeros (Some 0 :: l) (x + 1)

    let elide l =
      let rec aux ~elide = function
        | [] -> []
        | x :: xs when x >= 0 ->
          Some x :: aux ~elide xs
        | x :: xs when Some x = elide ->
          None :: aux ~elide:None xs
        | z :: xs ->
          cons_zeros (aux ~elide xs) z
      in
      let elide, l = calc_elide l in
      assert (match elide with Some x when x < -8 -> false | _ -> true);
      aux ~elide l

    (* Based on https://github.com/mirage/ocaml-ipaddr/
       See http://tools.ietf.org/html/rfc5952 *)
    let pp f t =
      let comp = to_int16 t in
      let v4 = match comp with [0; 0; 0; 0; 0; 0xffff; _; _] -> true | _ -> false in
      let l = elide comp in
      let rec fill = function
        | [ Some hi; Some lo ] when v4 ->
          Fmt.pf f "%d.%d.%d.%d"
            (hi lsr 8) (hi land 0xff)
            (lo lsr 8) (lo land 0xff)
        | None :: xs ->
          Fmt.string f "::";
          fill xs
        | [ Some n ] -> Fmt.pf f "%x" n
        | Some n :: None :: xs ->
          Fmt.pf f "%x::" n;
          fill xs
        | Some n :: xs ->
          Fmt.pf f "%x:" n;
          fill xs
        | [] -> ()
      in
      fill l
  end

  type v4v6 = [`V4 | `V6] t

  let fold ~v4 ~v6 t =
    match String.length t with
    | 4 -> v4 t
    | 16 -> v6 t
    | _ -> assert false

  let of_raw t =
    match String.length t with
    | 4 | 16 -> t
    | x -> Fmt.invalid_arg "An IP address must be either 4 or 16 bytes long (%S is %d bytes)" t x

  let pp f = fold ~v4:(V4.pp f) ~v6:(V6.pp f)

  let pp_for_uri f =
    fold
      ~v4:(V4.pp f)
      ~v6:(Fmt.pf f "[%a]" V6.pp)
end

module Sockaddr = struct
  type stream = [
    | `Unix of string
    | `Tcp of Ipaddr.v4v6 * int
  ]

  type datagram = [
    | `Udp of Ipaddr.v4v6 * int
    | `Unix of string
  ]

  type t = [ stream | datagram ]

  let pp f = function
    | `Unix path ->
      Format.fprintf f "unix:%s" path
    | `Tcp (addr, port) ->
      Format.fprintf f "tcp:%a:%d" Ipaddr.pp_for_uri addr port
    | `Udp (addr, port) ->
      Format.fprintf f "udp:%a:%d" Ipaddr.pp_for_uri addr port
end

type socket_ty = [`Socket | `Close]
type 'a socket = ([> socket_ty] as 'a) r

type 'tag stream_socket_ty = [`Stream | `Platform of 'tag | `Shutdown | socket_ty | Flow.source_ty | Flow.sink_ty]
type 'a stream_socket = 'a r
  constraint 'a = [> [> `Generic] stream_socket_ty]

type 'tag listening_socket_ty = [ `Accept | `Platform of 'tag | socket_ty]
type 'a listening_socket = 'a r
  constraint 'a = [> [> `Generic] listening_socket_ty]

type 'a connection_handler = 'a stream_socket -> Sockaddr.stream -> unit

type 'tag datagram_socket_ty = [`Datagram | `Platform of 'tag | `Shutdown | socket_ty]
type 'a datagram_socket = 'a r
  constraint 'a = [> [> `Generic] datagram_socket_ty]

type 'tag ty = [`Network | `Platform of 'tag]
type 'a t = 'a r
  constraint 'a = [> [> `Generic] ty]

module Pi = struct
  module type STREAM_SOCKET = sig
    type tag
    include Flow.Pi.SHUTDOWN
    include Flow.Pi.SOURCE with type t := t
    include Flow.Pi.SINK with type t := t
    val close : t -> unit
  end

  let stream_socket (type t tag) (module X : STREAM_SOCKET with type t = t and type tag = tag) =
    Resource.handler @@
    H (Resource.Close, X.close) ::
    Resource.bindings (Flow.Pi.two_way (module X))

  module type DATAGRAM_SOCKET = sig
    type tag
    include Flow.Pi.SHUTDOWN
    val send : t -> ?dst:Sockaddr.datagram -> Cstruct.t list -> unit
    val recv : t -> Cstruct.t -> Sockaddr.datagram * int
    val close : t -> unit
  end

  type (_, _, _) Resource.pi +=
    | Datagram_socket : ('t, (module DATAGRAM_SOCKET with type t = 't), [> _ datagram_socket_ty]) Resource.pi

  let datagram_socket (type t tag) (module X : DATAGRAM_SOCKET with type t = t and type tag = tag) =
    Resource.handler @@
    Resource.bindings (Flow.Pi.shutdown (module X)) @ [
      H (Datagram_socket, (module X));
      H (Resource.Close, X.close)
    ]

  module type LISTENING_SOCKET = sig
    type t
    type tag

    val accept : t -> sw:Switch.t -> tag stream_socket_ty r * Sockaddr.stream
    val close : t -> unit
    val listening_addr : t -> Sockaddr.stream
  end

  type (_, _, _) Resource.pi +=
    | Listening_socket : ('t, (module LISTENING_SOCKET with type t = 't and type tag = 'tag), [> 'tag listening_socket_ty]) Resource.pi

  let listening_socket (type t tag) (module X : LISTENING_SOCKET with type t = t and type tag = tag) =
    Resource.handler [
      H (Resource.Close, X.close);
      H (Listening_socket, (module X))
    ]

  module type NETWORK = sig
    type t
    type tag

    val listen : t -> reuse_addr:bool -> reuse_port:bool -> backlog:int -> sw:Switch.t -> Sockaddr.stream -> tag listening_socket_ty r
    val connect : t -> sw:Switch.t -> Sockaddr.stream -> tag stream_socket_ty r
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

  type (_, _, _) Resource.pi +=
    | Network : ('t, (module NETWORK with type t = 't and type tag = 'tag), [> 'tag ty]) Resource.pi

  let network (type t tag) (module X : NETWORK with type t = t and type tag = tag) =
    Resource.handler [
      H (Network, (module X));
    ]
end

let accept ~sw (type tag) (Resource.T (t, ops) : [> tag listening_socket_ty] r) =
  let module X = (val (Resource.get ops Pi.Listening_socket)) in
  X.accept t ~sw

let accept_fork ~sw (t : [> 'a listening_socket_ty] r) ~on_error handle =
  let child_started = ref false in
  let flow, addr = accept ~sw t in
  Fun.protect ~finally:(fun () -> if !child_started = false then Flow.close flow)
    (fun () ->
       Fiber.fork ~sw (fun () ->
           match child_started := true; handle (flow :> 'a stream_socket_ty r) addr with
           | x -> Flow.close flow; x
           | exception (Cancel.Cancelled _ as ex) ->
             Flow.close flow;
             raise ex
           | exception ex ->
             Flow.close flow;
             on_error (Exn.add_context ex "handling connection from %a" Sockaddr.pp addr)
         )
    )

let listening_addr (type tag) (Resource.T (t, ops) : [> tag listening_socket_ty] r) =
  let module X = (val (Resource.get ops Pi.Listening_socket)) in
  X.listening_addr t

let send (Resource.T (t, ops)) ?dst bufs =
  let module X = (val (Resource.get ops Pi.Datagram_socket)) in
  X.send t ?dst bufs

let recv (Resource.T (t, ops)) buf =
  let module X = (val (Resource.get ops Pi.Datagram_socket)) in
  X.recv t buf

let listen (type tag) ?(reuse_addr=false) ?(reuse_port=false) ~backlog ~sw (t:[> tag ty] r) =
  let (Resource.T (t, ops)) = t in
  let module X = (val (Resource.get ops Pi.Network)) in
  X.listen t ~reuse_addr ~reuse_port ~backlog ~sw

let connect (type tag) ~sw (t:[> tag ty] r) addr =
  let (Resource.T (t, ops)) = t in
  let module X = (val (Resource.get ops Pi.Network)) in
  try X.connect t ~sw addr
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "connecting to %a" Sockaddr.pp addr

let datagram_socket (type tag) ?(reuse_addr=false) ?(reuse_port=false) ~sw (t:[> tag ty] r) addr =
  let (Resource.T (t, ops)) = t in
  let module X = (val (Resource.get ops Pi.Network)) in
  let addr = (addr :> [Sockaddr.datagram | `UdpV4 | `UdpV6]) in 
  X.datagram_socket t ~reuse_addr ~reuse_port ~sw addr

let getaddrinfo (type tag) ?(service="") (t:[> tag ty] r) hostname =
  let (Resource.T (t, ops)) = t in
  let module X = (val (Resource.get ops Pi.Network)) in
  X.getaddrinfo t ~service hostname

let getaddrinfo_stream ?service t hostname =
  getaddrinfo ?service t hostname
  |> List.filter_map (function
      | #Sockaddr.stream as x -> Some x
      | _ -> None
    )

let getaddrinfo_datagram ?service t hostname =
  getaddrinfo ?service t hostname
  |> List.filter_map (function
      | #Sockaddr.datagram as x -> Some x
      | _ -> None
    )

let getnameinfo (type tag) (t:[> tag ty] r) sockaddr =
  let (Resource.T (t, ops)) = t in
  let module X = (val (Resource.get ops Pi.Network)) in
  X.getnameinfo t sockaddr

let close = Resource.close

let with_tcp_connect ?(timeout=Time.Timeout.none) ~host ~service t f =
  Switch.run ~name:"with_tcp_connect" @@ fun sw ->
  match
    let rec aux = function
      | [] -> raise @@ err (Connection_failure No_matching_addresses)
      | addr :: addrs ->
        try Time.Timeout.run_exn timeout (fun () -> connect ~sw t addr) with
        | Time.Timeout | Exn.Io _ when addrs <> [] ->
          aux addrs
        | Time.Timeout ->
          raise @@ err (Connection_failure Timeout)
    in
    getaddrinfo_stream ~service t host
    |> List.filter_map (function
        | `Tcp _ as x -> Some x
        | `Unix _ -> None
      )
    |> aux
  with
  | conn -> f conn
  | exception (Exn.Io _ as ex) ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "connecting to %S:%s" host service

(* Run a server loop in a single domain. *)
let run_server_loop ~sw ~connections ~on_error ~stop listening_socket connection_handler =
  let rec accept () =
    Semaphore.acquire connections;
    accept_fork ~sw ~on_error listening_socket (fun conn addr ->
        Fun.protect (fun () -> connection_handler conn addr)
            ~finally:(fun () -> Semaphore.release connections)
      );
    accept ()
  in
  match stop with
  | None -> accept ()
  | Some stop -> Fiber.first accept (fun () -> Promise.await stop)

let run_server ?(max_connections=Int.max_int) ?(additional_domains) ?stop ~on_error listening_socket connection_handler : 'a =
  if max_connections <= 0 then invalid_arg "max_connections";
  Switch.run ~name:"run_server" @@ fun sw ->
  let connections = Semaphore.make max_connections in
  let run_server_loop sw = run_server_loop ~sw ~connections ~on_error ~stop listening_socket connection_handler in
  additional_domains |> Option.iter (fun (domain_mgr, domains) ->
      if domains < 0 then invalid_arg "additional_domains";
      for _ = 1 to domains do
        Fiber.fork ~sw (fun () -> Domain_manager.run domain_mgr (fun () ->
            Switch.run ~name:"run_server" @@ fun sw ->
            ignore (run_server_loop sw : 'a)
          ))
      done;
    );
  run_server_loop sw
