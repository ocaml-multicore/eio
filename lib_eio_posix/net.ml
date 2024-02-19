open Eio.Std

module Fd = Eio_unix.Fd

let socket_domain_of = function
  | `Unix _ -> Unix.PF_UNIX
  | `UdpV4 -> Unix.PF_INET
  | `UdpV6 -> Unix.PF_INET6
  | `Udp (host, _)
  | `Tcp (host, _) ->
    Eio.Net.Ipaddr.fold host
      ~v4:(fun _ -> Unix.PF_INET)
      ~v6:(fun _ -> Unix.PF_INET6)

module Listening_socket = struct
  type t = {
    hook : Switch.hook;
    fd : Fd.t;
  }

  type tag = [`Generic | `Unix]

  let make ~hook fd = { hook; fd }

  let fd t = t.fd

  let close t =
    Switch.remove_hook t.hook;
    Fd.close t.fd

  let accept t ~sw =
    let client, client_addr = Err.run (Low_level.accept ~sw) t.fd in
    let client_addr = match client_addr with
      | Unix.ADDR_UNIX path         -> `Unix path
      | Unix.ADDR_INET (host, port) -> `Tcp (Eio_unix.Net.Ipaddr.of_unix host, port)
    in
    let flow = (Flow.of_fd client :> _ Eio.Net.stream_socket) in
    flow, client_addr

  let listening_addr { fd; _ } =
    Eio_unix.Fd.use_exn "listening_addr" fd
      (fun fd -> Eio_unix.Net.sockaddr_of_unix_stream (Unix.getsockname fd))
end

let listening_handler = Eio_unix.Pi.listening_socket_handler (module Listening_socket)

let listening_socket ~hook fd =
  Eio.Resource.T (Listening_socket.make ~hook fd, listening_handler)

module Datagram_socket = struct
  type tag = [`Generic | `Unix]

  type t = Eio_unix.Fd.t

  let close = Fd.close

  let fd t = t

  let send t ?dst buf =
    let dst = Option.map Eio_unix.Net.sockaddr_to_unix dst in
    let sent = Err.run (Low_level.send_msg t ?dst) (Array.of_list buf) in
    assert (sent = Cstruct.lenv buf)

  let recv t buf =
    let addr, recv = Err.run (Low_level.recv_msg t) [| buf |] in
    Eio_unix.Net.sockaddr_of_unix_datagram addr, recv

  let shutdown t cmd =
    try
      Low_level.shutdown t @@ match cmd with
      | `Receive -> Unix.SHUTDOWN_RECEIVE
      | `Send -> Unix.SHUTDOWN_SEND
      | `All -> Unix.SHUTDOWN_ALL
    with
    | Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()
    | Unix.Unix_error (code, name, arg) -> raise (Err.wrap code name arg)
end

let datagram_handler = Eio_unix.Pi.datagram_handler (module Datagram_socket)

let datagram_socket fd =
  Eio.Resource.T (fd, datagram_handler)

(* https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml *)
let getaddrinfo ~service node =
  let to_eio_sockaddr_t {Unix.ai_family; ai_addr; ai_socktype; ai_protocol; _ } =
    match ai_family, ai_socktype, ai_addr with
    | (Unix.PF_INET | PF_INET6),
      (Unix.SOCK_STREAM | SOCK_DGRAM),
      Unix.ADDR_INET (inet_addr,port) -> (
        match ai_protocol with
        | 6 -> Some (`Tcp (Eio_unix.Net.Ipaddr.of_unix inet_addr, port))
        | 17 -> Some (`Udp (Eio_unix.Net.Ipaddr.of_unix inet_addr, port))
        | _ -> None)
    | _ -> None
  in
  Err.run (Eio_unix.run_in_systhread ~label:"getaddrinfo") @@ fun () ->
  let rec aux () =
    try
      Unix.getaddrinfo node service []
      |> List.filter_map to_eio_sockaddr_t
    with Unix.Unix_error (EINTR, _, _) -> aux ()
  in
  aux ()

let listen ~reuse_addr ~reuse_port ~backlog ~sw (listen_addr : Eio.Net.Sockaddr.stream) =
  let socket_type, addr =
    match listen_addr with
    | `Unix path         ->
      if reuse_addr then (
        let buf = Low_level.create_stat () in
        match Low_level.fstatat ~buf ~follow:false Fs path with
        | () -> if Low_level.kind buf = `Socket then Unix.unlink path
        | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
        | exception Unix.Unix_error (code, name, arg) -> raise @@ Err.wrap code name arg
      );
      Unix.SOCK_STREAM, Unix.ADDR_UNIX path
    | `Tcp (host, port)  ->
      let host = Eio_unix.Net.Ipaddr.to_unix host in
      Unix.SOCK_STREAM, Unix.ADDR_INET (host, port)
  in
  let sock = Low_level.socket ~sw (socket_domain_of listen_addr) socket_type 0 in
  (* For Unix domain sockets, remove the path when done (except for abstract sockets). *)
  let hook =
    match listen_addr with
    | `Unix path when String.length path > 0 && path.[0] <> Char.chr 0 ->
      Switch.on_release_cancellable sw (fun () -> Unix.unlink path)
    | `Unix _ | `Tcp _ ->
      Switch.null_hook
  in
  Fd.use_exn "listen" sock (fun fd ->
      if reuse_addr then
        Unix.setsockopt fd Unix.SO_REUSEADDR true;
      if reuse_port then
        Unix.setsockopt fd Unix.SO_REUSEPORT true;
      Unix.bind fd addr;
      Unix.listen fd backlog;
    );
  (listening_socket ~hook sock :> _ Eio.Net.listening_socket_ty r)

let connect ~sw connect_addr =
  let socket_type, addr =
    match connect_addr with
    | `Unix path         -> Unix.SOCK_STREAM, Unix.ADDR_UNIX path
    | `Tcp (host, port)  ->
      let host = Eio_unix.Net.Ipaddr.to_unix host in
      Unix.SOCK_STREAM, Unix.ADDR_INET (host, port)
  in
  let sock = Low_level.socket ~sw (socket_domain_of connect_addr) socket_type 0 in
  try
    Low_level.connect sock addr;
    (Flow.of_fd sock :> _ Eio_unix.Net.stream_socket)
  with Unix.Unix_error (code, name, arg) -> raise (Err.wrap code name arg)

let create_datagram_socket ~reuse_addr ~reuse_port ~sw saddr =
  let sock = Low_level.socket ~sw (socket_domain_of saddr) Unix.SOCK_DGRAM 0 in
  begin match saddr with
    | `Udp _ | `Unix _ as saddr ->
      let addr = Eio_unix.Net.sockaddr_to_unix saddr in
      Fd.use_exn "datagram_socket" sock (fun fd ->
          if reuse_addr then
            Unix.setsockopt fd Unix.SO_REUSEADDR true;
          if reuse_port then
            Unix.setsockopt fd Unix.SO_REUSEPORT true;
          Unix.bind fd addr
        )
    | `UdpV4 | `UdpV6 -> ()
  end;
  datagram_socket sock

module Impl = struct
  type t = unit
  type tag = [`Generic | `Unix]

  let listen () = listen

  let connect () ~sw addr =
    let socket = connect ~sw addr in
    (socket :> [`Generic | `Unix] Eio.Net.stream_socket_ty r)

  let datagram_socket () ~reuse_addr ~reuse_port ~sw saddr =
    let socket = create_datagram_socket ~reuse_addr ~reuse_port ~sw saddr in
    (socket :> [`Generic | `Unix] Eio.Net.datagram_socket_ty r)

  let getaddrinfo () = getaddrinfo
  let getnameinfo () = Eio_unix.Net.getnameinfo
end

let v : Impl.tag Eio.Net.ty r =
  let handler = Eio.Net.Pi.network (module Impl) in
  Eio.Resource.T ((), handler)
