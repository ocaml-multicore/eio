open Eio.Std

type ty = [`Generic | `Mock] Eio.Net.ty
type t = ty r

module Impl = struct
  type tag = [`Generic]

  type t = {
    label : string;
    on_listen : tag Eio.Net.listening_socket_ty r Handler.t;
    on_connect : tag Eio.Net.stream_socket_ty r Handler.t;
    on_datagram_socket : tag Eio.Net.datagram_socket_ty r Handler.t;
    on_getaddrinfo : Eio.Net.Sockaddr.t list Handler.t;
    on_getnameinfo : (string * string) Handler.t;
  }

  let make label = {
    label;
    on_listen = Handler.make (`Raise (Failure "Mock listen handler not configured"));
    on_connect = Handler.make (`Raise (Failure "Mock connect handler not configured"));
    on_datagram_socket = Handler.make (`Raise (Failure "Mock datagram_socket handler not configured"));
    on_getaddrinfo = Handler.make (`Raise (Failure "Mock getaddrinfo handler not configured"));
    on_getnameinfo = Handler.make (`Raise (Failure "Mock getnameinfo handler not configured"));
  }

  let on_listen t = t.on_listen
  let on_connect t = t.on_connect
  let on_datagram_socket t = t.on_datagram_socket
  let on_getaddrinfo t = t.on_getaddrinfo
  let on_getnameinfo t = t.on_getnameinfo

  let listen t ~reuse_addr:_ ~reuse_port:_ ~backlog:_ ~sw addr =
    traceln "%s: listen on %a" t.label Eio.Net.Sockaddr.pp addr;
    let socket = Handler.run t.on_listen in
    Switch.on_release sw (fun () -> Eio.Resource.close socket);
    socket

  let connect t ~sw addr =
    traceln "%s: connect to %a" t.label Eio.Net.Sockaddr.pp addr;
    let socket = Handler.run t.on_connect in
    Switch.on_release sw (fun () -> Eio.Flow.close socket);
    socket

  let datagram_socket t ~reuse_addr:_ ~reuse_port:_ ~sw addr =
    (match addr with
     | #Eio.Net.Sockaddr.datagram as saddr -> traceln "%s: datagram_socket %a" t.label Eio.Net.Sockaddr.pp saddr
     | `UdpV4 -> traceln "%s: datagram_socket UDPv4" t.label
     | `UdpV6 -> traceln "%s: datagram_socket UDPv6" t.label
    );
    let socket = Handler.run t.on_datagram_socket in
    Switch.on_release sw (fun () -> Eio.Flow.close socket);
    socket

  let getaddrinfo t ~service node =
    traceln "%s: getaddrinfo ~service:%s %s" t.label service node;
    Handler.run t.on_getaddrinfo

  let getnameinfo t sockaddr =
    traceln "%s: getnameinfo %a" t.label Eio.Net.Sockaddr.pp sockaddr;
    Handler.run t.on_getnameinfo

  type (_, _, _) Eio.Resource.pi += Raw : ('t, 't -> t, ty) Eio.Resource.pi
  let raw (Eio.Resource.T (t, ops)) = Eio.Resource.get ops Raw t
end

let make : string -> t =
  let super = Eio.Net.Pi.network (module Impl) in
  let handler = Eio.Resource.handler (
      H (Impl.Raw, Fun.id) ::
      Eio.Resource.bindings super
    ) in
  fun label -> Eio.Resource.T (Impl.make label, handler)

let on_connect (t:t) actions =
  let t = Impl.raw t in
  let as_socket x = (x :> [`Generic] Eio.Net.stream_socket_ty r) in
  Handler.seq t.on_connect (List.map (Action.map as_socket) actions)

let on_listen (t:t) actions =
  let t = Impl.raw t in
  let as_socket x = (x :> [`Generic] Eio.Net.listening_socket_ty r) in
  Handler.seq t.on_listen (List.map (Action.map as_socket) actions)

let on_datagram_socket (t:t) (actions : _ r Handler.actions) =
  let t = Impl.raw t in
  let as_socket x = (x :> [`Generic] Eio.Net.datagram_socket_ty r) in
  Handler.seq t.on_datagram_socket (List.map (Action.map as_socket) actions)

let on_getaddrinfo (t:t) actions = Handler.seq (Impl.raw t).on_getaddrinfo actions

let on_getnameinfo (t:t) actions = Handler.seq (Impl.raw t).on_getnameinfo actions

type listening_socket_ty = [`Generic | `Mock] Eio.Net.listening_socket_ty
type listening_socket = listening_socket_ty r

module Listening_socket = struct
  type t = {
    label : string;
    listening_addr : Eio.Net.Sockaddr.stream;
    on_accept : (Flow.t * Eio.Net.Sockaddr.stream) Handler.t;
  }

  type tag = [`Generic]

  let make ?(listening_addr = `Tcp (Eio.Net.Ipaddr.V4.any, 0)) label =
    {
      label;
      listening_addr;
      on_accept = Handler.make (`Raise (Failure "Mock accept handler not configured"))
    }

  let on_accept t = t.on_accept

  let accept t ~sw =
      let socket, addr = Handler.run t.on_accept in
      Flow.attach_to_switch (socket : Flow.t) sw;
      traceln "%s: accepted connection from %a" t.label Eio.Net.Sockaddr.pp addr;
      (socket :> tag Eio.Net.stream_socket_ty r), addr

  let close t =
    traceln "%s: closed" t.label

  let listening_addr { listening_addr; _ } = listening_addr

  type (_, _, _) Eio.Resource.pi += Type : ('t, 't -> t, listening_socket_ty) Eio.Resource.pi
  let raw (Eio.Resource.T (t, ops)) = Eio.Resource.get ops Type t
end

let listening_socket_handler =
  Eio.Resource.handler @@
  Eio.Resource.bindings (Eio.Net.Pi.listening_socket (module Listening_socket)) @ [
    H (Listening_socket.Type, Fun.id);
  ]

let listening_socket ?listening_addr label : listening_socket =
  Eio.Resource.T (Listening_socket.make ?listening_addr label, listening_socket_handler)

let on_accept l actions =
  let r = Listening_socket.raw l in
  let as_accept_pair x = (x :> Flow.t * Eio.Net.Sockaddr.stream) in
  Handler.seq r.on_accept (List.map (Action.map as_accept_pair) actions)
