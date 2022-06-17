open Eio.Std

type t = <
  Eio.Net.t;
  on_listen : Eio.Net.listening_socket Handler.t;
  on_connect : Eio.Net.stream_socket Handler.t;
  on_datagram_socket : Eio.Net.datagram_socket Handler.t;
>

let make label =
  let on_listen = Handler.make (`Raise (Failure "Mock listen handler not configured")) in
  let on_connect = Handler.make (`Raise (Failure "Mock connect handler not configured")) in
  let on_datagram_socket = Handler.make (`Raise (Failure "Mock datagram_socket handler not configured")) in
  object
    inherit Eio.Net.t

    method on_listen = on_listen
    method on_connect = on_connect
    method on_datagram_socket = on_datagram_socket

    method listen ~reuse_addr:_ ~reuse_port:_ ~backlog:_ ~sw addr =
      traceln "%s: listen on %a" label Eio.Net.Sockaddr.pp addr;
      let socket = Handler.run on_listen in
      Switch.on_release sw (fun () -> Eio.Flow.close socket);
      socket

    method connect ~sw addr =
      traceln "%s: connect to %a" label Eio.Net.Sockaddr.pp addr;
      let socket = Handler.run on_connect in
      Switch.on_release sw (fun () -> Eio.Flow.close socket);
      socket

    method datagram_socket ~sw addr =
      traceln "%s: datagram_socket %a" label Eio.Net.Sockaddr.pp addr;
      let socket = Handler.run on_datagram_socket in
      Switch.on_release sw (fun () -> Eio.Flow.close socket);
      socket
  end

let on_connect (t:t) actions =
  let as_socket x = (x :> Eio.Net.stream_socket) in
  Handler.seq t#on_connect (List.map (Action.map as_socket) actions)

let on_listen (t:t) actions =
  let as_socket x = (x :> Eio.Net.listening_socket) in
  Handler.seq t#on_listen (List.map (Action.map as_socket) actions)

let on_datagram_socket (t:t) actions =
  let as_socket x = (x :> Eio.Net.datagram_socket) in
  Handler.seq t#on_datagram_socket (List.map (Action.map as_socket) actions)

type listening_socket = <
  Eio.Net.listening_socket;
  on_accept : (Eio.Net.stream_socket * Eio.Net.Sockaddr.stream) Handler.t;
>

let listening_socket label =
  let on_accept = Handler.make (`Raise (Failure "Mock accept handler not configured")) in
  object
    inherit Eio.Net.listening_socket

    method on_accept = on_accept

    method accept ~sw =
      let socket, addr = Handler.run on_accept in
      Switch.on_release sw (fun () -> Eio.Flow.close socket);
      traceln "%s: accepted connection from %a" label Eio.Net.Sockaddr.pp addr;
      socket, addr

    method close =
      traceln "%s: closed" label
  end

let on_accept (l:listening_socket) actions =
  let as_accept_pair x = (x :> Eio.Net.stream_socket * Eio.Net.Sockaddr.stream) in
  Handler.seq l#on_accept (List.map (Action.map as_accept_pair) actions)
