open Eio.Std

let read_all flow =
  let b = Buffer.create 100 in
  Eio.Flow.copy flow (Eio.Flow.buffer_sink b);
  Buffer.contents b

let run_client ~sw ~net ~addr =
  traceln "Connecting to server...";
  let flow = Eio.Net.connect ~sw net addr in
  Eio.traceln "connected";
  Eio.Flow.copy_string "Hello from client" flow;
  Eio.Flow.shutdown flow `Send;
  let msg = read_all flow in
  msg

let run_server ~sw msg socket =
  Eio.Net.accept_fork socket ~sw (fun flow _addr ->
    traceln "Server accepted connection from client";
    Fun.protect (fun () ->
      let msg = read_all flow in
      traceln "Server received: %S" msg
    ) ~finally:(fun () -> Eio.Flow.copy_string msg flow)
  )
  ~on_error:(function
    | ex -> traceln "Error handling connection: %s" (Printexc.to_string ex)
  )

let test_client_server env addr () =
  Eio.Switch.run @@ fun sw ->
  let server = Eio.Net.listen env#net ~sw ~reuse_addr:true ~backlog:5 addr in
  let msg = "From the server" in
  Fiber.both
    (fun () -> run_server ~sw msg server)
    (fun () ->
      let client_msg = run_client ~sw ~net:env#net ~addr in 
      Alcotest.(check string) "same message" msg client_msg
    )

let run_dgram addr ~net sw msg =
  let e1 = `Udp (addr, 8081) in
  let e2 = `Udp (addr, 8082) in
  let listening_socket = Eio.Net.datagram_socket ~sw net e2 in
  Fiber.both
    (fun () ->
      let buf = Cstruct.create 20 in
      traceln "Waiting to receive data on %a" Eio.Net.Sockaddr.pp e2;
      let addr, recv = Eio.Net.recv listening_socket buf in
      traceln "Received message from %a"
      Eio.Net.Sockaddr.pp addr;
      Alcotest.(check string) "same udp msg" msg (Cstruct.(to_string (sub buf 0 recv)))
    )
    (fun () ->
      let e = Eio.Net.datagram_socket ~sw net e1 in
      traceln "Sending data from %a to %a" Eio.Net.Sockaddr.pp e1 Eio.Net.Sockaddr.pp e2;
      Eio.Net.send e ~dst:e2 [Cstruct.of_string msg])

let test_udp env addr () =
  Eio.Switch.run @@ fun sw ->
  run_dgram addr ~net:env#net sw "UDP on Windows"

let test_fd env addr () =
  Eio.Switch.run @@ fun sw ->
  let addr = `Tcp (addr, 8081) in
  let server = Eio.Net.listen env#net ~sw ~reuse_addr:true ~backlog:5 addr in
  Alcotest.(check bool) "Listening socket has Unix FD" (Eio_unix.Resource.fd_opt server <> None) true;
  let have_client, have_server =
    Fiber.pair
      (fun () ->
         let flow = Eio.Net.connect ~sw env#net addr in
         (Eio_unix.Resource.fd_opt flow <> None)
      )
      (fun () ->
         let flow, _addr = Eio.Net.accept ~sw server in
         (Eio_unix.Resource.fd_opt flow <> None)
      )
  in
  Alcotest.(check bool) "Client-side socket has Unix FD" have_client true;
  Alcotest.(check bool) "Server-side socket has Unix FD" have_server true

let test_wrap_socket pipe_or_socketpair () =
  Switch.run @@ fun sw ->
  let r, w = 
    match pipe_or_socketpair with
    | `Pipe -> Unix.pipe ()
    | `Socketpair -> Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let source = (Eio_unix.Net.import_socket_stream ~sw ~close_unix:true r :> Eio.Flow.source_ty r) in
  let sink = (Eio_unix.Net.import_socket_stream ~sw ~close_unix:true w :> Eio.Flow.sink_ty r) in
  let msg = "Hello" in
  Fiber.both
    (fun () -> Eio.Flow.copy_string (msg ^ "\n") sink)
    (fun () ->
        let b = Eio.Buf_read.of_flow source ~max_size:1000 in
        Alcotest.(check string) "same message" (Eio.Buf_read.line b) msg
    )

let test_eio_socketpair () =
  Switch.run @@ fun sw ->
  let a, b = Eio_unix.Net.socketpair_stream ~sw () in
  ignore (Eio_unix.Net.fd a : Eio_unix.Fd.t);
  ignore (Eio_unix.Net.fd b : Eio_unix.Fd.t);
  Eio.Flow.copy_string "foo" a;
  Eio.Flow.close a;
  let msg = Eio.Buf_read.of_flow b ~max_size:10 |> Eio.Buf_read.take_all in
  Alcotest.(check string) "same messagw" "foo" msg

(* Copyright (C) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: ISC *)

let test_abstract_socket env () =
  Switch.run @@ fun sw ->
  let addr = `Unix "\000eio-test" in
  let rejected f =
    match f () with
    | _ -> Alcotest.fail "accepted an abstract Unix address"
    | exception Eio.Io (Eio.Exn.Not_available _, _) -> ()
  in
  List.iter (fun reuse_addr ->
      rejected (fun () -> Eio.Net.listen ~sw ~reuse_addr ~backlog:1 env#net addr)
    ) [false; true];
  rejected (fun () -> Eio.Net.connect ~sw env#net addr);
  rejected (fun () -> Eio.Net.connect ~sw ~bind_to:addr env#net (`Unix "no-server.sock"));
  rejected (fun () -> Eio.Net.datagram_socket ~sw env#net addr);
  let udp = Eio.Net.datagram_socket ~sw env#net `UdpV4 in
  rejected (fun () -> Eio.Net.send udp ~dst:addr [])

let test_resolve env () =
  let host = "127.0.0.1" in
  let lo = Eio.Net.Ipaddr.V4.loopback in
  let got = Eio.Net.getaddrinfo ~service:"80" env#net host in
  List.iter (fun addr -> Alcotest.(check bool) "numeric service" true (List.mem addr got))
    [`Tcp (lo, 80); `Udp (lo, 80)];
  Alcotest.(check bool) "TCP-only service" true
    (Eio.Net.getaddrinfo ~service:"http" env#net host = [`Tcp (lo, 80)]);
  Alcotest.(check bool) "IPv6 literal" true
    (List.mem (`Tcp (Eio.Net.Ipaddr.V6.loopback, 0)) (Eio.Net.getaddrinfo env#net "::1"));
  match Eio.Net.getaddrinfo ~service:"nosuchservice" env#net host with
  | _ -> Alcotest.fail "resolved an unknown service"
  | exception Eio.Io (Eio.Net.E (Address_lookup_failed SERVICE), _) -> ()

let test_unsupported_datagram env () =
  Switch.run @@ fun sw ->
  match Eio.Net.datagram_socket ~sw env#net (`Unix "unused.sock") with
  | _ -> Alcotest.fail "created an AF_UNIX datagram socket"
  | exception Eio.Io (_, _) -> ()

let tests env = [
  "unsupported datagram socket", `Quick, test_unsupported_datagram env;
  "resolver service and error codes", `Quick, test_resolve env;
  "tcp-ip4", `Quick, test_client_server env (`Tcp (Eio.Net.Ipaddr.V4.loopback, 8081));
  "tcp-ip6", `Quick, test_client_server env (`Tcp (Eio.Net.Ipaddr.V6.loopback, 8081));
  "unix", `Quick, test_client_server env (`Unix "eio-test.sock");
  "abstract Unix addresses", `Quick, test_abstract_socket env;
  "udp-ip4", `Quick, test_udp env Eio.Net.Ipaddr.V4.loopback;
  "udp-ip6", `Quick, test_udp env Eio.Net.Ipaddr.V6.loopback;
  "fds", `Quick, test_fd env Eio.Net.Ipaddr.V4.loopback;
  "wrap-pipe", `Quick, test_wrap_socket `Pipe;
  "wrap-socketpair", `Quick, test_wrap_socket `Socketpair;
  "eio-socketpair", `Quick, test_eio_socketpair
]
