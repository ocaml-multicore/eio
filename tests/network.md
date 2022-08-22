## Setting up the environment

```ocaml
# #require "eio_main";;
# #require "eio.mock";;
```

```ocaml
open Eio.Std

let run (fn : net:Eio.Net.t -> Switch.t -> unit) =
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
      Eio.Net.send e e2 (Cstruct.of_string "UDP Message"))
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

## Unix interop

Extracting file descriptors from Eio objects:

```ocaml
# run @@ fun ~net sw ->
  let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  traceln "Listening socket has Unix FD: %b" (Eio_unix.FD.peek_opt server <> None);
  let have_client, have_server =
    Fiber.pair
      (fun () -> 
         let flow = Eio.Net.connect ~sw net addr in
         (Eio_unix.FD.peek_opt flow <> None)
      )
      (fun () ->
         let flow, _addr = Eio.Net.accept ~sw server in
         (Eio_unix.FD.peek_opt flow <> None)
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
# Eio.Net.Ipaddr.V4.loopback |> Eio_unix.Ipaddr.to_unix |> Unix.string_of_inet_addr;;
- : string = "127.0.0.1"
# Eio.Net.Ipaddr.V4.any |> Eio_unix.Ipaddr.to_unix |> Unix.string_of_inet_addr;;
- : string = "0.0.0.0"
# Eio.Net.Ipaddr.V6.loopback |> Eio_unix.Ipaddr.to_unix |> Unix.string_of_inet_addr;;
- : string = "::1"
# Eio.Net.Ipaddr.V6.any |> Eio_unix.Ipaddr.to_unix |> Unix.string_of_inet_addr;;
- : string = "::"
```

Check we can convert Unix IP addresses to Eio:

```ocaml
# Eio_main.run @@ fun _ ->
  let show x = traceln "%a" Eio.Net.Ipaddr.pp (Eio_unix.Ipaddr.of_unix (Unix.inet_addr_of_string x)) in
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
    let host = Eio_unix.Ipaddr.of_unix (Unix.inet_addr_of_string host) in
    traceln "%a" Eio.Net.Sockaddr.pp (`Tcp (host, port))
  in
  Eio_main.run @@ fun env ->
  show "127.0.0.1" 8080;
  show "::1" 8080;;
+tcp:127.0.0.1:8080
+tcp:[::1]:8080
- : unit = ()
```

Wrapping a Unix FD as an Eio socket:

```ocaml
# Eio_main.run @@ fun _ ->
  Switch.run @@ fun sw ->
  let r, w = Unix.pipe () in
  let source = (Eio_unix.FD.as_socket ~sw ~close_unix:true r :> Eio.Flow.source) in
  let sink = (Eio_unix.FD.as_socket ~sw ~close_unix:true w :> Eio.Flow.sink) in
  Fiber.both
    (fun () -> Eio.Flow.copy_string "Hello\n!" sink)
    (fun () ->
       let b = Eio.Buf_read.of_flow source ~max_size:1000 in
       traceln "Got: %S" (Eio.Buf_read.line b)
    );;
+Got: "Hello"
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

## Cancelling multiple jobs

We start two jobs and cancel both. Cancellation happens in series. By the time the second job's cancel function is called, it has already finished.

```ocaml
(* Accepts a connection when cancelled.
   The cancellation doesn't finish until the client operation has succeeded. *)
let mock_cancellable ~sw ~server ~set_client_ready =
  Eio.Private.Suspend.enter (fun ctx enqueue ->
    Eio.Private.Fiber_context.set_cancel_fn ctx (fun ex ->
      try
        traceln "Cancelled. Accepting connection...";
        let _flow, _addr = Eio.Net.accept ~sw server in
        (* Can't trace here because client might resume first *)
        (* traceln "Accepted connection."; *)
        let p, r = Promise.create () in
        Promise.resolve set_client_ready (Promise.resolve r);
        Promise.await p;
        (* The client successfully connected. We now allow this cancel
           function to complete, which will cause the connect operation's
           cancel function to be called next. *)
        enqueue (Error ex)
      with ex ->
        traceln "Cancel function failed!";
        enqueue (Error ex)
    )
  )
```

```ocaml
# Eio_main.run @@ fun env ->
  (* Logs.set_reporter (Logs_fmt.reporter ()); *)
  (* Logs.set_level (Some Logs.Debug); *)
  let net = env#net in
  Switch.run @@ fun sw ->
  let client_ready, set_client_ready = Promise.create () in
  let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  Fiber.all [
    (fun () ->
       (* Start an operation that will be cancelled first, to add a delay. *)
       mock_cancellable ~sw ~server ~set_client_ready
     );
    (fun () ->
       (* Start a connect operation. It won't be accepted until the previous
          fiber is cancelled. *)
       match Eio.Net.connect ~sw net addr with
       | _flow ->
         traceln "Client connected";
         Eio.Cancel.protect (fun () -> Promise.await client_ready ())
       | exception ex ->
         traceln "Failed: %a" Fmt.exn ex;
         Eio.Cancel.protect (fun () -> Promise.await client_ready ())
    );
    (fun () ->
       (* Cancel both fibers. Cancelling the first allows the second to complete,
          even though it's also cancelled. *)
       failwith "Simulated error"
    );
  ];;
+Cancelled. Accepting connection...
+Client connected
Exception: Failure "Simulated error".
```

## Socketpair

```ocaml
# Eio_main.run @@ fun _ ->
  Switch.run @@ fun sw ->
  let a, b = Eio_unix.socketpair ~sw () in
  ignore (Eio_unix.FD.peek a : Unix.file_descr);
  ignore (Eio_unix.FD.peek b : Unix.file_descr);
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
  let a, b = Eio_unix.socketpair ~sw () in
  Eio.Flow.copy_string "foo" a;
  Eio.Flow.close b;     (* Close without reading *)
  try
    ignore (Eio.Flow.read a (Cstruct.create 1) : int);
    assert false
  with Eio.Net.Connection_reset _ -> traceln "Connection failed (good)";;
+Connection failed (good)
- : unit = ()
```

EPIPE:

```ocaml
# Eio_main.run @@ fun _ ->
  Switch.run @@ fun sw ->
  let a, b = Eio_unix.socketpair ~sw () in
  Eio.Flow.close b;
  try
    Eio.Flow.copy_string "foo" a;
    assert false
  with Eio.Net.Connection_reset _ -> traceln "Connection failed (good)";;
+Connection failed (good)
- : unit = ()
```

## Shutdown

```ocaml
# Eio_main.run @@ fun _ ->
  Switch.run @@ fun sw ->
  let a, b = Eio_unix.socketpair ~sw () in
  Fiber.both
    (fun () ->
       match Eio.Flow.read a (Cstruct.create 1) with
       | (_ : int) -> failwith "Should have ended!"
       | exception End_of_file -> ()
    )
    (fun () -> Eio.Flow.shutdown a `Receive);;
- : unit = ()
```

## Getaddrinfo

```ocaml
# Eio_main.run @@ fun env ->
  Eio.Net.getaddrinfo env#net "127.0.0.1";;
- : Eio.Net.Sockaddr.t list =
[`Tcp ("\127\000\000\001", 0); `Udp ("\127\000\000\001", 0)]
```

```ocaml
# Eio_main.run @@ fun env ->
  Eio.Net.getaddrinfo env#net "127.0.0.1" ~service:"80";;
- : Eio.Net.Sockaddr.t list =
[`Tcp ("\127\000\000\001", 80); `Udp ("\127\000\000\001", 80)]
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
[`Tcp ("ь:тн", 443); `Udp ("ь:тн", 443);
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
