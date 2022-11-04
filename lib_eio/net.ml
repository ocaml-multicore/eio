exception Connection_reset of exn
(** This is a wrapper for EPIPE, ECONNRESET and similar errors.
    It indicates that the flow has failed, and data may have been lost. *)

exception Connection_failure of exn

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

class virtual socket = object (_ : #Generic.t)
  method probe _ = None
end

class virtual stream_socket = object
  inherit Flow.two_way
end

class virtual listening_socket = object
  inherit socket
  method virtual accept : sw:Switch.t -> <stream_socket; Flow.close> * Sockaddr.stream
  method virtual close : unit
end

let accept ~sw (t : #listening_socket) = t#accept ~sw

let accept_fork ~sw (t : #listening_socket) ~on_error handle =
  let child_started = ref false in
  let flow, addr = accept ~sw t in
  Fun.protect ~finally:(fun () -> if !child_started = false then Flow.close flow)
    (fun () ->
       Fiber.fork ~sw (fun () ->
           match child_started := true; handle (flow :> stream_socket) addr with
           | x -> Flow.close flow; x
           | exception ex ->
             Flow.close flow;
             on_error ex
         )
    )

let accept_sub ~sw (t : #listening_socket) ~on_error handle =
  accept_fork ~sw t ~on_error (fun flow addr -> Switch.run (fun sw -> handle ~sw flow addr))

class virtual datagram_socket = object
  inherit socket
  method virtual send : Sockaddr.datagram -> Cstruct.t -> unit
  method virtual recv : Cstruct.t -> Sockaddr.datagram * int
end

let send (t:#datagram_socket) = t#send
let recv (t:#datagram_socket) = t#recv

class virtual t = object
  method virtual listen : reuse_addr:bool -> reuse_port:bool -> backlog:int -> sw:Switch.t -> Sockaddr.stream -> listening_socket
  method virtual connect : sw:Switch.t -> Sockaddr.stream -> <stream_socket; Flow.close>
  method virtual datagram_socket :
       reuse_addr:bool
    -> reuse_port:bool
    -> sw:Switch.t
    -> [Sockaddr.datagram | `UdpV4 | `UdpV6]
    -> <datagram_socket; Flow.close>

  method virtual getaddrinfo : service:string -> string -> Sockaddr.t list
  method virtual getnameinfo : Sockaddr.t -> (string * string)
end

let listen ?(reuse_addr=false) ?(reuse_port=false) ~backlog ~sw (t:#t) = t#listen ~reuse_addr ~reuse_port ~backlog ~sw
let connect ~sw (t:#t) = t#connect ~sw

let datagram_socket ?(reuse_addr=false) ?(reuse_port=false) ~sw (t:#t) addr =
  let addr = (addr :> [Sockaddr.datagram | `UdpV4 | `UdpV6]) in 
  t#datagram_socket ~reuse_addr ~reuse_port ~sw addr

let getaddrinfo ?(service="") (t:#t) hostname = t#getaddrinfo ~service hostname

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

let getnameinfo (t:#t) sockaddr = t#getnameinfo sockaddr

let close = Flow.close

let with_tcp_connect ?(timeout=Time.Timeout.none) ~host ~service t f =
  Switch.run @@ fun sw ->
  let rec aux = function
    | [] -> raise (Connection_failure (Failure (Fmt.str "No TCP addresses for %S" host)))
    | addr :: addrs ->
      match Time.Timeout.run_exn timeout (fun () -> connect ~sw t addr) with
      | conn -> f conn
      | exception (Time.Timeout | Connection_failure _) when addrs <> [] ->
        aux addrs
      | exception (Connection_failure _ as ex) ->
        raise ex
      | exception (Time.Timeout as ex) ->
        raise (Connection_failure ex)
  in
  getaddrinfo_stream ~service t host
  |> List.filter_map (function
      | `Tcp _ as x -> Some x
      | `Unix _ -> None
    )
  |> aux
