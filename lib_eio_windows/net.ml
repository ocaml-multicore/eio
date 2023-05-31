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

let listening_socket ~hook fd = object
  inherit Eio.Net.listening_socket

  method close =
    Switch.remove_hook hook;
    Fd.close fd

  method accept ~sw =
    let client, client_addr = Err.run (Low_level.accept ~sw) fd in
    let client_addr = match client_addr with
      | Unix.ADDR_UNIX path         -> `Unix path
      | Unix.ADDR_INET (host, port) -> `Tcp (Eio_unix.Net.Ipaddr.of_unix host, port)
    in
    let flow = (Flow.of_fd client :> <Eio.Net.stream_socket; Eio.Flow.close>) in
    flow, client_addr

  method! probe : type a. a Eio.Generic.ty -> a option = function
    | Eio_unix.Resource.FD -> Some fd
    | _ -> None
end

(* todo: would be nice to avoid copying between bytes and cstructs here *)
let datagram_socket sock = object
  inherit Eio_unix.Net.datagram_socket

  method close = Fd.close sock

  method fd = sock

  method send ?dst buf =
    let dst = Option.map Eio_unix.Net.sockaddr_to_unix dst in
    let sent = Err.run (Low_level.send_msg sock ?dst) (Bytes.unsafe_of_string (Cstruct.copyv buf)) in
    assert (sent = Cstruct.lenv buf)

  method recv buf =
    let b = Bytes.create (Cstruct.length buf) in
    let recv, addr = Err.run (Low_level.recv_msg sock) b in
    Cstruct.blit_from_bytes b 0 buf 0 recv;
    Eio_unix.Net.sockaddr_of_unix_datagram addr, recv
end

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
  Err.run Eio_unix.run_in_systhread @@ fun () ->
  let rec aux () =
    try
      Unix.getaddrinfo node service []
      |> List.filter_map to_eio_sockaddr_t
    with Unix.Unix_error (EINTR, _, _) -> aux ()
  in
  aux ()

let listen ~reuse_addr ~reuse_port ~backlog ~sw (listen_addr : Eio.Net.Sockaddr.stream) =
  let socket_type, addr, is_unix_socket =
    match listen_addr with
    | `Unix path         ->
      if reuse_addr then (
        match Low_level.lstat path with
        | Unix.{ st_kind = S_SOCK; _ } -> Unix.unlink path
        | _ -> ()
        | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
        | exception Unix.Unix_error (code, name, arg) -> raise @@ Err.wrap code name arg
      );
      Unix.SOCK_STREAM, Unix.ADDR_UNIX path, true
    | `Tcp (host, port)  ->
      let host = Eio_unix.Net.Ipaddr.to_unix host in
      Unix.SOCK_STREAM, Unix.ADDR_INET (host, port), false
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
      (* REUSEADDR cannot be set on a Windows UNIX domain socket,
         otherwise the Unix.bind will fail! *)
      if not is_unix_socket && reuse_addr then
        Unix.setsockopt fd Unix.SO_REUSEADDR true;
      if reuse_port then
        Unix.setsockopt fd Unix.SO_REUSEPORT true;
      Unix.bind fd addr;
      Unix.listen fd backlog
    );
  listening_socket ~hook sock

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
    (Flow.of_fd sock :> <Eio.Net.stream_socket; Eio.Flow.close>)
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
  (datagram_socket sock :> <Eio.Net.datagram_socket; Eio.Flow.close>)

let v = object
  inherit Eio_unix.Net.t

  method listen = listen
  method connect = connect
  method datagram_socket = create_datagram_socket
  method getaddrinfo = getaddrinfo
end
