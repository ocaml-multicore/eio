open Eio.Std

module Fd = Eio_unix.Fd

let check_address = function
  | `Unix path when String.starts_with ~prefix:"\000" path ->
    raise (Err.v EOPNOTSUPP "abstract Unix socket" path)
  | _ -> ()

let socket_domain_of addr =
  check_address addr;
  match addr with
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
    hook : Switch.hook;                 (* Deletes the path, for Unix-domain sockets *)
    fd : Fd.t;
  }

  type tag = [`Generic | `Unix]

  let make ~hook fd = { hook; fd }

  let fd t = t.fd

  let close t =
    Switch.remove_hook ~run:true t.hook;
    Fd.close t.fd

  let accept t ~sw =
    let client, client_addr = Err.run (Low_level.accept ~sw) t.fd in
    let flow = (Flow.stream client :> _ Eio.Net.stream_socket) in
    flow, Eio_unix.Net.sockaddr_of_unix_stream client_addr

  let listening_addr { fd; _ } =
    Eio_unix.Fd.use_exn "listening_addr" fd
      (fun fd -> Eio_unix.Net.sockaddr_of_unix_stream (Unix.getsockname fd))

  let setsockopt t opt v = Eio_unix.Private.setsockopt t.fd opt v
  let getsockopt t opt = Eio_unix.Private.getsockopt t.fd opt
end

let listening_handler = Eio_unix.Pi.listening_socket_handler (module Listening_socket)

let listening_socket ~hook fd =
  Eio.Resource.T (Listening_socket.make ~hook fd, listening_handler)

module Datagram_socket = struct
  type tag = [`Generic | `Unix]

  type t = {
    hook : Switch.hook;                 (* Deletes the path, for Unix-domain sockets *)
    fd : Fd.t;
  }

  let make ~hook fd = { hook; fd }

  let close t =
    Switch.remove_hook ~run:true t.hook;
    Fd.close t.fd

  let fd t = t.fd

  let send t ?dst buf =
    Option.iter check_address dst;
    let dst = Option.map Eio_unix.Net.sockaddr_to_unix dst in
    let buf = Cstruct.concat buf in
    let sent = Err.run (Datagram.send t.fd ?dst) buf in
    assert (sent = Cstruct.length buf)

  let recv t buf =
    let recv, addr = Err.run (Datagram.recv t.fd) buf in
    Eio_unix.Net.sockaddr_of_unix_datagram addr, recv

  let shutdown t cmd =
    try
      Fd.use_exn "shutdown" t.fd @@ fun raw ->
      match cmd with
      | `Send -> Completion.shutdown raw Unix.SHUTDOWN_SEND
      | `Receive | `All ->
        match Unix.getpeername raw with
        | _ -> Completion.shutdown raw (if cmd = `All then Unix.SHUTDOWN_ALL else Unix.SHUTDOWN_RECEIVE)
        | exception Unix.Unix_error (ENOTCONN, _, _) ->
          (* An unconnected datagram socket can still receive from any peer. *)
          if cmd = `All then Completion.shutdown raw Unix.SHUTDOWN_SEND
    with
    | Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()
    | Unix.Unix_error (code, name, arg) -> raise (Err.v code name arg)

  let setsockopt t opt v = Eio_unix.Private.setsockopt t.fd opt v
  let getsockopt t opt = Eio_unix.Private.getsockopt t.fd opt
end

let datagram_handler = Eio_unix.Pi.datagram_handler (module Datagram_socket)

let datagram_socket ?(hook=Switch.null_hook) fd =
  Eio.Resource.T (Datagram_socket.make ~hook fd, datagram_handler)

let getaddrinfo ~service node =
  Eio_unix.run_in_systhread ~label:"getaddrinfo" @@ fun () ->
  Err.run (Eio_unix.Private.getaddrinfo ~service) node

let remove_on_release ~sw listen_addr =
  (* For Unix domain sockets, remove the path when done (except for abstract sockets). *)
  match listen_addr with
  | `Unix path when String.length path > 0 && path.[0] <> Char.chr 0 ->
    Switch.on_release_cancellable sw (fun () ->
        try Unix.unlink path
        with Unix.Unix_error _ ->
          (* Ignore error: cleaning up the socket isn't essential, and maybe someone deleted it. *)
          ()
      )
  | _ ->
    Switch.null_hook

let socket ~sw addr ty = Err.run (Low_level.socket ~sw (socket_domain_of addr) ty) 0

let listen ~reuse_addr ~reuse_port ~backlog ~sw (listen_addr : Eio.Net.Sockaddr.stream) =
  let socket_type, addr, is_unix_socket =
    match listen_addr with
    | `Unix path         ->
      if reuse_addr then (
        check_address listen_addr;
        match Low_level.lstat path with
        | Unix.{ st_kind = S_SOCK; _ } -> Unix.unlink path
        | _ -> ()
        | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
        | exception Unix.Unix_error (code, name, arg) -> raise @@ Err.v code name arg
      );
      Unix.SOCK_STREAM, Unix.ADDR_UNIX path, true
    | `Tcp (host, port)  ->
      let host = Eio_unix.Net.Ipaddr.to_unix host in
      Unix.SOCK_STREAM, Unix.ADDR_INET (host, port), false
  in
  let sock = socket ~sw listen_addr socket_type in
  let hook =
    Fd.use_exn "listen" sock (fun fd ->
        try
          (* REUSEADDR cannot be set on a Windows UNIX domain socket,
             otherwise the Unix.bind will fail! *)
          if not is_unix_socket && reuse_addr then
            Unix.setsockopt fd Unix.SO_REUSEADDR true;
          if reuse_port then
            Unix.setsockopt fd Unix.SO_REUSEPORT true;
          Unix.bind fd addr;
          let hook = remove_on_release ~sw listen_addr in
          Unix.listen fd backlog;
          hook
        with Unix.Unix_error (code, name, arg) ->
          raise (Eio_unix.Err.v code name arg)
      )
  in
  (listening_socket ~hook sock :> _ Eio.Net.listening_socket_ty r)

let connect ~bind_to ~options ~sw connect_addr =
  Option.iter check_address bind_to;
  let socket_type, addr =
    match connect_addr with
    | `Unix path         -> Unix.SOCK_STREAM, Unix.ADDR_UNIX path
    | `Tcp (host, port)  ->
      let host = Eio_unix.Net.Ipaddr.to_unix host in
      Unix.SOCK_STREAM, Unix.ADDR_INET (host, port)
  in
  let sock = socket ~sw connect_addr socket_type in
  let rec set_options : Eio.Net.Sockopt.settings -> unit = function
    | [] -> ()
    | (o, v) :: tl -> Eio_unix.Private.setsockopt sock o v; set_options tl
  in
  Err.run (fun () ->
      set_options options;
      Option.iter (fun a -> Fd.use_exn "bind" sock (fun fd -> Unix.bind fd (Eio_unix.Net.sockaddr_to_unix a))) bind_to;
      Low_level.connect sock addr
    ) ();
  (Flow.stream sock :> _ Eio_unix.Net.stream_socket)

let create_datagram_socket ~reuse_addr ~reuse_port ~sw saddr =
  let sock = socket ~sw saddr Unix.SOCK_DGRAM in
  let hook =
    match saddr with
    | `Udp _ | `Unix _ as saddr ->
      let addr = Eio_unix.Net.sockaddr_to_unix saddr in
      Fd.use_exn "datagram_socket" sock (fun fd ->
          try
            if reuse_addr then
              Unix.setsockopt fd Unix.SO_REUSEADDR true;
            if reuse_port then
              Unix.setsockopt fd Unix.SO_REUSEPORT true;
            Unix.bind fd addr;
            remove_on_release ~sw saddr;
          with Unix.Unix_error (code, name, arg) ->
            raise (Eio_unix.Err.v code name arg)
        )
    | `UdpV4 | `UdpV6 -> Switch.null_hook
  in
  datagram_socket ~hook sock

module Impl = struct
  type t = unit
  type tag = [`Generic | `Unix]

  let listen () = listen

  let connect () ~bind_to ~options ~sw addr =
    let socket = connect ~bind_to ~options ~sw addr in
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
