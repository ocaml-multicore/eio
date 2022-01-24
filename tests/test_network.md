# Setting up the environment

```ocaml
# #require "eio_main";;
```

```ocaml
open Eio.Std

let run (fn : net:Eio.Net.t -> Switch.t -> unit) =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Switch.run (fn ~net)

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8081)

let read_all flow =
  let b = Buffer.create 100 in
  Eio.Flow.copy flow (Eio.Flow.buffer_sink b);
  Buffer.contents b

exception Graceful_shutdown
```

# Test cases

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
    Eio.Net.accept_sub socket ~sw (fun ~sw flow _addr ->
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
  Fibre.both
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
# run (test_address (`Unix "/tmp/eio-test.sock"));;
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

Cancelling the read:

```ocaml
# run @@ fun ~net sw ->
  let shutdown, set_shutdown = Promise.create () in
  let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  Fibre.both
    (fun () ->
        Eio.Net.accept_sub server ~sw (fun ~sw flow _addr ->
          try
            Fibre.both
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
      Fibre.yield ();
      Promise.fulfill set_shutdown Graceful_shutdown;
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
  Eio.Net.accept_sub server ~sw (fun ~sw:_ _flow _addr -> assert false)
    ~on_error:raise;;
Exception: Failure "Simulated error".
```
# Unix interop

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
