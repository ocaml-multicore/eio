## Setting up the environment

```ocaml
# #require "eio_main";;
# #require "eio.mock";;
```

```ocaml
open Eio.Std

let run (fn : net:_ Eio.Net.t -> Switch.t -> unit) =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Switch.run (fn ~net)

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8081)
let addr6 = `Tcp (Eio.Net.Ipaddr.V6.loopback, 8081)

let read_all flow =
  let b = Buffer.create 100 in
  Eio.Flow.copy flow (Eio.Flow.buffer_sink b);
  Buffer.contents b

exception Graceful_shutdown

let () = Eio.Exn.Backend.show := false
```

## Test cases

A simple client:

```ocaml
let run_client ~sw ~net ~addr =
  traceln "Connecting to server...";
  let flow = Eio.Net.connect ~sw net addr in
  Eio.Flow.copy_string "Hello from client" flow;
  Eio.Flow.shutdown flow `Send;
  let msg = read_all flow in
  traceln "Client received: %S" msg
```

A simple server:

```ocaml
let run_server ~sw socket =
  while true do
    Eio.Net.accept_fork socket ~sw (fun flow _addr ->
      traceln "Server accepted connection from client";
      Fun.protect (fun () ->
        let msg = read_all flow in
        traceln "Server received: %S" msg
      ) ~finally:(fun () -> Eio.Flow.copy_string "Bye" flow)
    )
    ~on_error:(function
      | Graceful_shutdown -> ()
      | ex -> traceln "Error handling connection: %s" (Printexc.to_string ex)
    );
  done

let test_address addr ~net sw =
  let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  Fiber.both
    (fun () -> run_server ~sw server)
    (fun () ->
      run_client ~sw ~net ~addr;
      traceln "Client finished - cancelling server";
      raise Graceful_shutdown
    )
```

Handling one connection, then cancelling the server:

```ocaml
# run (test_address addr);;
+Connecting to server...
+Server accepted connection from client
+Server received: "Hello from client"
+Client received: "Bye"
+Client finished - cancelling server
Exception: Graceful_shutdown.
```

Handling one connection on a Unix domain socket:

```ocaml
# run (test_address (`Unix "eio-test.sock"));;
+Connecting to server...
+Server accepted connection from client
+Server received: "Hello from client"
+Client received: "Bye"
+Client finished - cancelling server
Exception: Graceful_shutdown.
```

Handling one connection on an abstract Unix domain socket (this only works on Linux):

<!-- $MDX non-deterministic=command -->
```ocaml
# run (test_address (`Unix "\x00/tmp/eio-test.sock"));;
+Connecting to server...
+Server accepted connection from client
+Server received: "Hello from client"
+Client received: "Bye"
+Client finished - cancelling server
Exception: Graceful_shutdown.
```

Handling one connection using IPv6:

```ocaml
# run (test_address addr6);;
+Connecting to server...
+Server accepted connection from client
+Server received: "Hello from client"
+Client received: "Bye"
+Client finished - cancelling server
Exception: Graceful_shutdown.
```

Cancelling the read:

```ocaml
# run @@ fun ~net sw ->
  let shutdown, set_shutdown = Promise.create () in
  let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  Fiber.both
    (fun () ->
        Eio.Net.accept_fork server ~sw (fun flow _addr ->
          try
            Fiber.both
              (fun () -> raise (Promise.await shutdown))
              (fun () ->
                let msg = read_all flow in
                traceln "Server received: %S" msg
              )
          with Graceful_shutdown ->
            Eio.Flow.copy_string "Request cancelled" flow
        ) ~on_error:raise
    )
    (fun () ->
      traceln "Connecting to server...";
      let flow = Eio.Net.connect ~sw net addr in
      traceln "Connection opened - cancelling server's read";
      Fiber.yield ();
      Promise.resolve set_shutdown Graceful_shutdown;
      let msg = read_all flow in
      traceln "Client received: %S" msg
    );;
+Connecting to server...
+Connection opened - cancelling server's read
+Client received: "Request cancelled"
- : unit = ()
```

Calling accept when the switch is already off:

```ocaml
# run @@ fun ~net sw ->
  let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  Switch.fail sw (Failure "Simulated error");
  Eio.Net.accept_fork server ~sw (fun _flow _addr -> assert false)
    ~on_error:raise;;
Exception: Failure "Simulated error".
```

Working with UDP and endpoints:

```ocaml
let run_dgram addr ~net sw =
  let e1 = `Udp (addr, 8081) in
  let e2 = `Udp (addr, 8082) in
  let listening_socket = Eio.Net.datagram_socket ~sw net e2 in
  Fiber.both
    (fun () ->
      let buf = Cstruct.create 20 in
      traceln "Waiting to receive data on %a" Eio.Net.Sockaddr.pp e2;
      let addr, recv = Eio.Net.recv listening_socket buf in
      traceln "Received message from %a: %s"
      Eio.Net.Sockaddr.pp addr
      (Cstruct.(to_string (sub buf 0 recv)))
    )
    (fun () ->
      let e = Eio.Net.datagram_socket ~sw net e1 in
      traceln "Sending data from %a to %a" Eio.Net.Sockaddr.pp e1 Eio.Net.Sockaddr.pp e2;
      Eio.Net.send e ~dst:e2 [Cstruct.of_string "UDP Message"])
```

Handling one UDP packet using IPv4:

```ocaml
# run (run_dgram Eio.Net.Ipaddr.V4.loopback);;
+Waiting to receive data on udp:127.0.0.1:8082
+Sending data from udp:127.0.0.1:8081 to udp:127.0.0.1:8082
+Received message from udp:127.0.0.1:8081: UDP Message
- : unit = ()
```

Handling one UDP packet using IPv6:

```ocaml
# run (run_dgram Eio.Net.Ipaddr.V6.loopback);;
+Waiting to receive data on udp:[::1]:8082
+Sending data from udp:[::1]:8081 to udp:[::1]:8082
+Received message from udp:[::1]:8081: UDP Message
- : unit = ()
```

Now test host-assigned addresses.
`run_dgram2` is like `run_dgram` above, but doesn't print the sender address
since it will be different in each run:

```ocaml
let run_dgram2 ~e1 addr ~net sw =
  let server_addr = `Udp (addr, 8082) in
  let listening_socket = Eio.Net.datagram_socket ~sw net server_addr in
  Fiber.both
    (fun () ->
      let buf = Cstruct.create 20 in
      traceln "Waiting to receive data on %a" Eio.Net.Sockaddr.pp server_addr;
      let addr, recv = Eio.Net.recv listening_socket buf in
      traceln "Received message %s" (Cstruct.(to_string (sub buf 0 recv)))
    )
    (fun () ->
      let e = Eio.Net.datagram_socket ~sw net e1 in
      traceln "Sending data to %a" Eio.Net.Sockaddr.pp server_addr;
      Eio.Net.send e ~dst:server_addr [Cstruct.of_string "UDP Message"]);;
```

Handling one UDP packet using IPv4:

```ocaml
# let addr = Eio.Net.Ipaddr.V4.loopback in
  run @@ run_dgram2 addr ~e1:`UdpV4;;
+Waiting to receive data on udp:127.0.0.1:8082
+Sending data to udp:127.0.0.1:8082
+Received message UDP Message
- : unit = ()
```

Handling one UDP packet using IPv6:

```ocaml
# let addr = Eio.Net.Ipaddr.V6.loopback in
  run @@ run_dgram2 addr ~e1:`UdpV6;;
+Waiting to receive data on udp:[::1]:8082
+Sending data to udp:[::1]:8082
+Received message UDP Message
- : unit = ()
```

It's not an error to close the socket before the handler returns:

```ocaml
# run @@ fun ~net sw ->
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8083) in
  let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  Fiber.both
    (fun () ->
        Eio.Net.accept_fork server ~sw ~on_error:raise @@ fun flow _addr ->
        traceln "Server got connection";
        Eio.Flow.copy_string "Hi" flow;
        Eio.Flow.close flow
    )
    (fun () ->
      traceln "Connecting to server...";
      let flow = Eio.Net.connect ~sw net addr in
      let msg = Eio.Buf_read.(parse_exn take_all) flow ~max_size:100 in
      traceln "Client got %S" msg;
    );;
+Connecting to server...
+Server got connection
+Client got "Hi"
- : unit = ()
```

## Unix interop

Extracting file descriptors from Eio objects:

```ocaml
# run @@ fun ~net sw ->
  let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  traceln "Listening socket has Unix FD: %b" (Eio_unix.Resource.fd_opt server <> None);
  let have_client, have_server =
    Fiber.pair
      (fun () ->
         let flow = Eio.Net.connect ~sw net addr in
         (Eio_unix.Resource.fd_opt flow <> None)
      )
      (fun () ->
         let flow, _addr = Eio.Net.accept ~sw server in
         (Eio_unix.Resource.fd_opt flow <> None)
      )
  in
  traceln "Client-side socket has Unix FD: %b" have_client;
  traceln "Server-side socket has Unix FD: %b" have_server;;
+Listening socket has Unix FD: true
+Client-side socket has Unix FD: true
+Server-side socket has Unix FD: true
- : unit = ()
```

Check we can convert Eio IP addresses to Unix:

```ocaml
# Eio.Net.Ipaddr.V4.loopback |> Eio_unix.Net.Ipaddr.to_unix |> Unix.string_of_inet_addr;;
- : string = "127.0.0.1"
# Eio.Net.Ipaddr.V4.any |> Eio_unix.Net.Ipaddr.to_unix |> Unix.string_of_inet_addr;;
- : string = "0.0.0.0"
# Eio.Net.Ipaddr.V6.loopback |> Eio_unix.Net.Ipaddr.to_unix |> Unix.string_of_inet_addr;;
- : string = "::1"
# Eio.Net.Ipaddr.V6.any |> Eio_unix.Net.Ipaddr.to_unix |> Unix.string_of_inet_addr;;
- : string = "::"
```

Check we can convert Unix IP addresses to Eio:

```ocaml
# Eio_main.run @@ fun _ ->
  let show x = traceln "%a" Eio.Net.Ipaddr.pp (Eio_unix.Net.Ipaddr.of_unix (Unix.inet_addr_of_string x)) in
  show "127.0.0.1";
  show "0.0.0.0";
  show "1234:5678:9abc:def0:fedc:ba98:7654:3210";
  show "::1";
  show "::";
  show "ab::";
  show "::ffff:192.168.1.3";
  show "1:0:0:2:0:0:0:3";
  show "4:1:0:0:2:0:0:3";;
+127.0.0.1
+0.0.0.0
+1234:5678:9abc:def0:fedc:ba98:7654:3210
+::1
+::
+ab::
+::ffff:192.168.1.3
+1:0:0:2::3
+4:1::2:0:0:3
- : unit = ()
```

Printing addresses with ports:

```ocaml
# let show host port =
    let host = Eio_unix.Net.Ipaddr.of_unix (Unix.inet_addr_of_string host) in
    traceln "%a" Eio.Net.Sockaddr.pp (`Tcp (host, port))
  in
  Eio_main.run @@ fun env ->
  show "127.0.0.1" 8080;
  show "::1" 8080;;
+tcp:127.0.0.1:8080
+tcp:[::1]:8080
- : unit = ()
```

Wrapping a Unix FD as an Eio stream socket:

```ocaml
# Eio_main.run @@ fun _ ->
  Switch.run @@ fun sw ->
  let r, w = Unix.pipe () in
  let source = (Eio_unix.Net.import_socket_stream ~sw ~close_unix:true r :> _ Eio.Flow.source) in
  let sink = (Eio_unix.Net.import_socket_stream ~sw ~close_unix:true w :> _ Eio.Flow.sink) in
  Fiber.both
    (fun () -> Eio.Flow.copy_string "Hello\n!" sink)
    (fun () ->
       let b = Eio.Buf_read.of_flow source ~max_size:1000 in
       traceln "Got: %S" (Eio.Buf_read.line b)
    );;
+Got: "Hello"
- : unit = ()
```

Wrapping a Unix FD as a listening Eio socket:

```ocaml
# run @@ fun ~net sw ->
  let l = Unix.(socket PF_INET SOCK_STREAM 0) in
  Unix.bind l (Unix.ADDR_INET (Unix.inet_addr_loopback, 8082));
  Unix.listen l 40;
  let l = Eio_unix.Net.import_socket_listening ~sw ~close_unix:true l in
  Fiber.both
    (fun () -> run_server ~sw l)
    (fun () ->
      run_client ~sw ~net ~addr:(`Tcp (Eio.Net.Ipaddr.V4.loopback, 8082));
      traceln "Client finished - cancelling server";
      raise Graceful_shutdown
    );;
+Connecting to server...
+Server accepted connection from client
+Server received: "Hello from client"
+Client received: "Bye"
+Client finished - cancelling server
Exception: Graceful_shutdown.
```

Wrapping a Unix FD as an datagram Eio socket:

```ocaml
# Eio_main.run @@ fun _ ->
  Switch.run @@ fun sw ->
  let a, b = Unix.(socketpair PF_UNIX SOCK_DGRAM 0) in
  let a = Eio_unix.Net.import_socket_datagram ~sw ~close_unix:true a in
  let b = Eio_unix.Net.import_socket_datagram ~sw ~close_unix:true b in
  Fiber.both
    (fun () -> Eio.Net.send a Cstruct.[of_string "12"; of_string "34"])
    (fun () ->
       let buf = Cstruct.create 10 in
       let addr, len = Eio.Net.recv b buf in
       traceln "Got: %S" (Cstruct.to_string buf ~len)
    );;
+Got: "1234"
- : unit = ()
```

## Accept_fork error handling

On success, we close the connection immediately:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let socket = Eio_mock.Net.listening_socket "tcp/80" in
  let flow = Eio_mock.Flow.make "connection" in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 1234) in
  Eio_mock.Net.on_accept socket [`Return (flow, addr)];
  Switch.run @@ fun sw ->
  Eio.Net.accept_fork ~sw ~on_error:raise socket
    (fun _flow _addr -> ());
  traceln "Mock connection should have been closed by now";;
+tcp/80: accepted connection from tcp:127.0.0.1:1234
+connection: closed
+Mock connection should have been closed by now
- : unit = ()
```
If the forked fiber fails, we close immediately:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let socket = Eio_mock.Net.listening_socket "tcp/80" in
  let flow = Eio_mock.Flow.make "connection" in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 1234) in
  Eio_mock.Net.on_accept socket [`Return (flow, addr)];
  Switch.run @@ fun sw ->
  Eio.Net.accept_fork ~sw ~on_error:raise socket
    (fun _flow _addr -> failwith "Simulated error");
  traceln "Mock connection should have been closed by now";;
+tcp/80: accepted connection from tcp:127.0.0.1:1234
+connection: closed
+Mock connection should have been closed by now
Exception: Failure "Simulated error".
```
If the fork itself fails, we still close the connection:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let socket = Eio_mock.Net.listening_socket "tcp/80" in
  let flow = Eio_mock.Flow.make "connection" in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 1234) in
  Eio_mock.Net.on_accept socket [`Return (flow, addr)];
  Switch.run @@ fun sw ->
  Switch.fail sw (Failure "Simulated error");
  Eio.Net.accept_fork ~sw ~on_error:raise socket
    (fun _flow _addr -> assert false);
  traceln "Mock connection should have been closed by now";;
+tcp/80: accepted connection from tcp:127.0.0.1:1234
+connection: closed
+Mock connection should have been closed by now
Exception: Failure "Simulated error".
```

`accept_fork` doesn't send cancellations to `on_error`:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let socket = Eio_mock.Net.listening_socket "tcp/80" in
  let flow = Eio_mock.Flow.make "connection" in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 1234) in
  Eio_mock.Net.on_accept socket [`Return (flow, addr)];
  Switch.run @@ fun sw ->
  Eio.Net.accept_fork ~sw ~on_error:(traceln "BUG: %a" Fmt.exn) socket
    (fun _flow _addr -> Fiber.await_cancel ());
  Switch.fail sw (Failure "Simulated error");;
+tcp/80: accepted connection from tcp:127.0.0.1:1234
+connection: closed
Exception: Failure "Simulated error".
```

## Socketpair

```ocaml
# Eio_main.run @@ fun _ ->
  Switch.run @@ fun sw ->
  let a, b = Eio_unix.Net.socketpair_stream ~sw () in
  ignore (Eio_unix.Net.fd a : Eio_unix.Fd.t);
  ignore (Eio_unix.Net.fd b : Eio_unix.Fd.t);
  Eio.Flow.copy_string "foo" a;
  Eio.Flow.close a;
  let msg = Eio.Buf_read.of_flow b ~max_size:10 |> Eio.Buf_read.take_all in
  traceln "Got: %S" msg;;
+Got: "foo"
- : unit = ()
```
## Errors

ECONNRESET:

```ocaml
# Eio_main.run @@ fun _ ->
  Switch.run @@ fun sw ->
  let a, b = Eio_unix.Net.socketpair_stream ~sw () in
  Eio.Flow.copy_string "foo" a;
  Eio.Flow.close b;     (* Close without reading *)
  try
    Eio.Flow.read_exact a (Cstruct.create 1);
    assert false
  with
  | Eio.Io (Eio.Net.E Connection_reset _, _)
  | End_of_file -> traceln "Connection failed (good)";;
+Connection failed (good)
- : unit = ()
```

EPIPE:

```ocaml
# Eio_main.run @@ fun _ ->
  Switch.run @@ fun sw ->
  let a, b = Eio_unix.Net.socketpair_stream ~sw () in
  Eio.Flow.close b;
  try
    Eio.Flow.copy_string "foo" a;
    assert false
  with Eio.Io (Eio.Net.E Connection_reset _, _) -> traceln "Connection failed (good)";;
+Connection failed (good)
- : unit = ()
```

Connection refused:

```ocaml
# Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  Eio.Net.connect ~sw env#net (`Unix "idontexist.sock");;
Exception: Eio.Io Fs Not_found _,
  connecting to unix:idontexist.sock
```

## Shutdown

```ocaml
# Eio_main.run @@ fun _ ->
  Switch.run @@ fun sw ->
  let a, b = Eio_unix.Net.socketpair_stream ~sw () in
  Fiber.both
    (fun () ->
       match Eio.Flow.read_exact a (Cstruct.create 1) with
       | () -> failwith "Should have ended!"
       | exception End_of_file -> ()
    )
    (fun () -> Eio.Flow.shutdown a `Receive);;
- : unit = ()
```

## Getaddrinfo

```ocaml
# Eio_main.run @@ fun env ->
  Eio.Net.getaddrinfo_stream env#net "127.0.0.1";;
- : Eio.Net.Sockaddr.stream list = [`Tcp ("\127\000\000\001", 0)]
```

```ocaml
# Eio_main.run @@ fun env ->
  Eio.Net.getaddrinfo_stream env#net "127.0.0.1" ~service:"80";;
- : Eio.Net.Sockaddr.stream list = [`Tcp ("\127\000\000\001", 80)]
```

```ocaml
# Eio_main.run @@ fun env ->
  Eio.Net.getaddrinfo_datagram env#net "127.0.0.1";;
- : Eio.Net.Sockaddr.datagram list = [`Udp ("\127\000\000\001", 0)]
```

```ocaml
# Eio_main.run @@ fun env ->
  Eio.Net.getaddrinfo_datagram env#net "127.0.0.1" ~service:"80";;
- : Eio.Net.Sockaddr.datagram list = [`Udp ("\127\000\000\001", 80)]
```

<!-- $MDX non-deterministic=output -->
```ocaml
# Eio_main.run @@ fun env ->
  Eio.Net.getaddrinfo ~service:"http" env#net "127.0.0.1";;
- : Eio.Net.Sockaddr.t list =
[`Tcp ("\127\000\000\001", 80); `Udp ("\127\000\000\001", 80)]
```

<!-- $MDX non-deterministic=output -->
```ocaml
# Eio_main.run @@ fun env ->
  Eio.Net.getaddrinfo ~service:"ftp" env#net "127.0.0.1";;
- : Eio.Net.Sockaddr.t list =
[`Tcp ("\127\000\000\001", 21); `Udp ("\127\000\000\001", 21)]
```

<!-- $MDX non-deterministic=output -->
```ocaml
# Eio_main.run @@ fun env ->
  Eio.Net.getaddrinfo ~service:"https" env#net "google.com";;
- : Eio.Net.Sockaddr.t list =
[`Tcp ("�:��", 443); `Udp ("�:��", 443);
 `Tcp ("*\000\020P@\t\b \000\000\000\000\000\000 \014", 443);
 `Udp ("*\000\020P@\t\b \000\000\000\000\000\000 \014", 443)]
```

## getnameinfo

```ocaml
# Eio_main.run @@ fun env ->
  let sockaddr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 80) in
  Eio.Net.getnameinfo env#net sockaddr;;
- : string * string = ("localhost", "http")
```

## with_tcp_connet

```ocaml
let net = Eio_mock.Net.make "mock-net"
let addr1 = `Tcp (Eio.Net.Ipaddr.V4.loopback, 80)
let addr2 = `Tcp (Eio.Net.Ipaddr.of_raw "\001\002\003\004", 8080)
let connection_failure = Eio.Net.err (Connection_failure (Refused Eio_mock.Simulated_failure))
```

No usable addresses:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Eio_mock.Net.on_getaddrinfo net [`Return [`Unix "/foo"]];
  Eio.Net.with_tcp_connect ~host:"www.example.com" ~service:"http" net (fun _ -> assert false);;
+mock-net: getaddrinfo ~service:http www.example.com
Exception:
Eio.Io Net Connection_failure No_matching_addresses,
  connecting to "www.example.com":http
```

First address works:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Eio_mock.Net.on_getaddrinfo net [`Return [addr1; addr2]];
  let mock_flow = Eio_mock.Flow.make "flow" in
  Eio_mock.Net.on_connect net [`Return mock_flow];
  Eio.Net.with_tcp_connect ~host:"www.example.com" ~service:"http" net (fun conn ->
     let req = "GET / HTTP/1.1\r\nHost:www.example.com:80\r\n\r\n" in
     Eio.Flow.copy_string req conn
  );;
+mock-net: getaddrinfo ~service:http www.example.com
+mock-net: connect to tcp:127.0.0.1:80
+flow: wrote "GET / HTTP/1.1\r\n"
+            "Host:www.example.com:80\r\n"
+            "\r\n"
+flow: closed
- : unit = ()
```

Second address works:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Eio_mock.Net.on_getaddrinfo net [`Return [addr1; addr2]];
  let mock_flow = Eio_mock.Flow.make "flow" in
  Eio_mock.Net.on_connect net [`Raise connection_failure;
                               `Return mock_flow];
  Eio.Net.with_tcp_connect ~host:"www.example.com" ~service:"http" net (fun conn ->
     let req = "GET / HTTP/1.1\r\nHost:www.example.com:80\r\n\r\n" in
     Eio.Flow.copy_string req conn
  );;
+mock-net: getaddrinfo ~service:http www.example.com
+mock-net: connect to tcp:127.0.0.1:80
+mock-net: connect to tcp:1.2.3.4:8080
+flow: wrote "GET / HTTP/1.1\r\n"
+            "Host:www.example.com:80\r\n"
+            "\r\n"
+flow: closed
- : unit = ()
```

Both addresses fail:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Eio_mock.Net.on_getaddrinfo net [`Return [addr1; addr2]];
  Eio_mock.Net.on_connect net [`Raise connection_failure; `Raise connection_failure];
  Eio.Net.with_tcp_connect ~host:"www.example.com" ~service:"http" net (fun _ -> assert false);;
+mock-net: getaddrinfo ~service:http www.example.com
+mock-net: connect to tcp:127.0.0.1:80
+mock-net: connect to tcp:1.2.3.4:8080
Exception:
Eio.Io Net Connection_failure Refused _,
  connecting to tcp:1.2.3.4:8080,
  connecting to "www.example.com":http
```

First attempt times out:

```ocaml
# Eio_mock.Backend.run_full @@ fun env ->
  let clock = env#mono_clock in
  let timeout = Eio.Time.Timeout.seconds clock 10. in
  Eio_mock.Net.on_getaddrinfo net [`Return [addr1; addr2]];
  let mock_flow = Eio_mock.Flow.make "flow" in
  Eio_mock.Net.on_connect net [`Run Fiber.await_cancel; `Return mock_flow];
  Eio.Net.with_tcp_connect ~timeout ~host:"www.example.com" ~service:"http" net (fun conn ->
     let req = "GET / HTTP/1.1\r\nHost:www.example.com:80\r\n\r\n" in
     Eio.Flow.copy_string req conn
  )
+mock-net: getaddrinfo ~service:http www.example.com
+mock-net: connect to tcp:127.0.0.1:80
+mock time is now 10
+mock-net: connect to tcp:1.2.3.4:8080
+flow: wrote "GET / HTTP/1.1\r\n"
+            "Host:www.example.com:80\r\n"
+            "\r\n"
+flow: closed
- : unit = ()
```

Both attempts time out:

```ocaml
# Eio_mock.Backend.run_full @@ fun env ->
  let clock = env#mono_clock in
  let timeout = Eio.Time.Timeout.seconds clock 10. in
  Eio_mock.Net.on_getaddrinfo net [`Return [addr1; addr2]];
  Eio_mock.Net.on_connect net [`Run Fiber.await_cancel; `Run Fiber.await_cancel];
  Eio.Net.with_tcp_connect ~timeout ~host:"www.example.com" ~service:"http" net (fun _ ->
    assert false
  )
+mock-net: getaddrinfo ~service:http www.example.com
+mock-net: connect to tcp:127.0.0.1:80
+mock time is now 10
+mock-net: connect to tcp:1.2.3.4:8080
+mock time is now 20
Exception:
Eio.Io Net Connection_failure Timeout,
  connecting to "www.example.com":http
```

## read/write on SOCK_DGRAM

```ocaml
# Eio_main.run @@ fun _ ->
  Switch.run @@ fun sw ->
  let a, b = Eio_unix.Net.socketpair_datagram ~sw ~domain:Unix.PF_UNIX () in
  ignore (Eio_unix.Net.fd a : Eio_unix.Fd.t);
  ignore (Eio_unix.Net.fd b : Eio_unix.Fd.t);
  let l = [ "foo"; "bar"; "foobar"; "cellar door"; "" ] in
  let buf = Cstruct.create 32 in
  let write bufs = Eio.Net.send a (List.map Cstruct.of_string bufs) in
  let read () =
    let _addr, n = Eio.Net.recv b buf in
    traceln "Got: %d bytes: %S" n Cstruct.(to_string (sub buf 0 n))
  in
  List.iter (fun sbuf -> write [sbuf]) l;
  List.iter (fun _ -> read ()) l;
  write ["abaca"; "bb"];
  read ();
  Eio.Flow.close a;
  Eio.Flow.close b;;
+Got: 3 bytes: "foo"
+Got: 3 bytes: "bar"
+Got: 6 bytes: "foobar"
+Got: 11 bytes: "cellar door"
+Got: 0 bytes: ""
+Got: 7 bytes: "abacabb"
- : unit = ()
```


## run_server

A simple connection handler for testing:
```ocaml
let handle_connection flow _addr =
  let msg = read_all flow in
  assert (msg = "Hi");
  Fiber.yield ();
  Eio.Flow.copy_string "Bye" flow
```

A mock listening socket that allows acceping `n_clients` clients, each of which writes "Hi",
and then allows `n_domains` further attempts, none of which ever completes:

```ocaml
let mock_listener ~n_clients ~n_domains =
  let make_flow i () =
    if n_domains > 1 then Fiber.yield ()       (* Load balance *)
    else Fiber.check ();
    let flow = Eio_mock.Flow.make ("flow" ^ string_of_int i) in
    Eio_mock.Flow.on_read flow [`Return "Hi"; `Raise End_of_file];
    flow, `Tcp (Eio.Net.Ipaddr.V4.loopback, 30000 + i)
  in
  let listening_socket = Eio_mock.Net.listening_socket "tcp/80" in
  Eio_mock.Net.on_accept listening_socket (
    List.init n_clients (fun i -> `Run (make_flow i)) @
    List.init n_domains (fun _ -> `Run Fiber.await_cancel)
  );
  listening_socket
```

Start handling the connections, then begin a graceful shutdown,
allowing the connections to finish and then exiting:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let listening_socket = mock_listener ~n_clients:3 ~n_domains:1 in
  let stop, set_stop = Promise.create () in
  Fiber.both
    (fun () ->
       Eio.Net.run_server listening_socket handle_connection
         ~max_connections:10
         ~on_error:raise
         ~stop
    )
    (fun () ->
       traceln "Begin graceful shutdown";
       Promise.resolve set_stop ()
    );;
+tcp/80: accepted connection from tcp:127.0.0.1:30000
+flow0: read "Hi"
+tcp/80: accepted connection from tcp:127.0.0.1:30001
+flow1: read "Hi"
+tcp/80: accepted connection from tcp:127.0.0.1:30002
+flow2: read "Hi"
+Begin graceful shutdown
+flow0: wrote "Bye"
+flow0: closed
+flow1: wrote "Bye"
+flow1: closed
+flow2: wrote "Bye"
+flow2: closed
- : unit = ()
```

Non-graceful shutdown, closing all connections still in progress:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let listening_socket = mock_listener ~n_clients:3 ~n_domains:1 in
  Fiber.both
    (fun () ->
      Eio.Net.run_server listening_socket handle_connection
        ~max_connections:10
        ~on_error:raise
    )
    (fun () -> failwith "Simulated error");;
+tcp/80: accepted connection from tcp:127.0.0.1:30000
+flow0: read "Hi"
+tcp/80: accepted connection from tcp:127.0.0.1:30001
+flow1: read "Hi"
+tcp/80: accepted connection from tcp:127.0.0.1:30002
+flow2: read "Hi"
+flow0: closed
+flow1: closed
+flow2: closed
Exception: Failure "Simulated error".
```

Handling the connections with 3 domains, with a graceful shutdown:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Eio_mock.Domain_manager.run @@ fun fake_domain_mgr ->
  let n_domains = 3 in
  let listening_socket = mock_listener ~n_clients:10 ~n_domains in
  let stop, set_stop = Promise.create () in
  Fiber.both
    (fun () ->
      Eio.Net.run_server listening_socket handle_connection
        ~additional_domains:(fake_domain_mgr, n_domains - 1)
        ~max_connections:10
        ~on_error:raise
        ~stop
    )
    (fun () ->
      Fiber.yield ();
      Promise.resolve set_stop ();
      Fiber.yield ();   (* Allow fibers to receive shutdown request *)
      traceln "Requested graceful shutdown"
    );;
+[1] tcp/80: accepted connection from tcp:127.0.0.1:30000
+[1] flow0: read "Hi"
+[2] tcp/80: accepted connection from tcp:127.0.0.1:30001
+[2] flow1: read "Hi"
+[0] tcp/80: accepted connection from tcp:127.0.0.1:30002
+[0] flow2: read "Hi"
+[1] flow0: wrote "Bye"
+[1] flow0: closed
+[1] tcp/80: accepted connection from tcp:127.0.0.1:30003
+[1] flow3: read "Hi"
+[2] flow1: wrote "Bye"
+[2] flow1: closed
+[2] tcp/80: accepted connection from tcp:127.0.0.1:30004
+[2] flow4: read "Hi"
+[0] flow2: wrote "Bye"
+[0] flow2: closed
+[0] tcp/80: accepted connection from tcp:127.0.0.1:30005
+[0] flow5: read "Hi"
+[0] Requested graceful shutdown
+[1] flow3: wrote "Bye"
+[1] flow3: closed
+[2] flow4: wrote "Bye"
+[2] flow4: closed
+[0] flow5: wrote "Bye"
+[0] flow5: closed
- : unit = ()
```

Handling the connections with 3 domains, aborting immediately:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Eio_mock.Domain_manager.run @@ fun fake_domain_mgr ->
  let n_domains = 3 in
  let listening_socket = mock_listener ~n_clients:10 ~n_domains in
  Fiber.both
    (fun () ->
      Eio.Net.run_server listening_socket handle_connection
        ~additional_domains:(fake_domain_mgr, n_domains - 1)
        ~max_connections:10
        ~on_error:raise
    )
    (fun () -> Fiber.yield (); failwith "Simulated error");;
+[1] tcp/80: accepted connection from tcp:127.0.0.1:30000
+[1] flow0: read "Hi"
+[2] tcp/80: accepted connection from tcp:127.0.0.1:30001
+[2] flow1: read "Hi"
+[0] tcp/80: accepted connection from tcp:127.0.0.1:30002
+[0] flow2: read "Hi"
+[1] flow0: closed
+[2] flow1: closed
+[0] flow2: closed
Exception: Failure "Simulated error".
```

Limiting to 2 concurrent connections:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let listening_socket = mock_listener ~n_clients:10 ~n_domains:1 in
  let stop, set_stop = Promise.create () in
  Fiber.both
    (fun () ->
      Eio.Net.run_server listening_socket handle_connection
        ~max_connections:2
        ~on_error:raise
        ~stop
    )
    (fun () ->
      for _ = 1 to 2 do Fiber.yield () done;
      traceln "Begin graceful shutdown";
      Promise.resolve set_stop ()
    );;
+tcp/80: accepted connection from tcp:127.0.0.1:30000
+flow0: read "Hi"
+tcp/80: accepted connection from tcp:127.0.0.1:30001
+flow1: read "Hi"
+flow0: wrote "Bye"
+flow0: closed
+flow1: wrote "Bye"
+flow1: closed
+tcp/80: accepted connection from tcp:127.0.0.1:30002
+flow2: read "Hi"
+tcp/80: accepted connection from tcp:127.0.0.1:30003
+flow3: read "Hi"
+Begin graceful shutdown
+flow2: wrote "Bye"
+flow2: closed
+flow3: wrote "Bye"
+flow3: closed
- : unit = ()
```

We keep the polymorphism when using a Unix network:

```ocaml
let _check_types ~(net:Eio_unix.Net.t) =
  Switch.run @@ fun sw ->
  let addr = `Unix "/socket" in
  let server : [`Generic | `Unix] Eio.Net.listening_socket_ty r =
    Eio.Net.listen ~sw net addr ~backlog:5
  in
  Eio.Net.accept_fork ~sw ~on_error:raise server
    (fun (_flow : [`Generic | `Unix] Eio.Net.stream_socket_ty r) _addr -> assert false);
  let _client : [`Generic | `Unix] Eio.Net.stream_socket_ty r = Eio.Net.connect ~sw net addr in
  ();;
```
