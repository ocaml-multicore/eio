# Setting up the environment

```ocaml
# #require "eio_main";;
```

```ocaml
open Eio.Std

let run (fn : net:Eio.Net.t -> Switch.t -> unit) =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Switch.top (fn ~net)

let addr = `Tcp (Unix.inet_addr_loopback, 8081)

let read_all ?sw flow =
  let b = Buffer.create 100 in
  Eio.Flow.copy ?sw flow (Eio.Flow.buffer_sink b);
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
  let msg = read_all ~sw flow in
  traceln "Client received: %S" msg
```

A simple server:

```ocaml
let run_server ~sw socket =
  while true do
    Eio.Net.accept_sub socket ~sw (fun ~sw flow _addr ->
      traceln "Server accepted connection from client";
      Fun.protect (fun () ->
        let msg = read_all ~sw flow in
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
  Fibre.both ~sw
    (fun () -> run_server ~sw server)
    (fun () ->
      run_client ~sw ~net ~addr;
      traceln "Client finished - cancelling server";
      Switch.turn_off sw Graceful_shutdown
    )
```

Handling one connection, then cancelling the server:

```ocaml
# run (test_address addr)
+Connecting to server...
+Server accepted connection from client
+Server received: "Hello from client"
+Client received: "Bye"
+Client finished - cancelling server
Exception: Graceful_shutdown.
```

Handling one connection on a Unix domain socket:

```ocaml
# run (test_address (`Unix "/tmp/eio-test.sock"))
+Connecting to server...
+Server accepted connection from client
+Server received: "Hello from client"
+Client received: "Bye"
+Client finished - cancelling server
Exception: Graceful_shutdown.
```

Handling one connection on an abstract Unix domain socket:

```ocaml
# run (test_address (`Unix "\x00/tmp/eio-test.sock"))
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
  Switch.top @@ fun read_switch ->
  let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  Fibre.both ~sw
    (fun () ->
      Eio.Net.accept_sub server ~sw (fun ~sw flow _addr ->
        try
          let msg = read_all ~sw:read_switch flow in
          traceln "Server received: %S" msg
        with Switch.Cancelled Graceful_shutdown ->
          Eio.Flow.copy_string "Request cancelled" flow
      ) ~on_error:raise
    )
    (fun () ->
      traceln "Connecting to server...";
      let flow = Eio.Net.connect ~sw net addr in
      traceln "Connection opened - cancelling server's read";
      Fibre.yield ();
      Switch.turn_off read_switch Graceful_shutdown;
      let msg = read_all flow in
      traceln "Client received: %S" msg
    )
+Connecting to server...
+Connection opened - cancelling server's read
+Client received: "Request cancelled"
Exception: Graceful_shutdown.
```

Calling accept when the switch is already off:

```ocaml
# run @@ fun ~net sw ->
  let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  Switch.turn_off sw (Failure "Simulated error");
  Eio.Net.accept_sub server ~sw (fun ~sw:_ _flow _addr -> assert false)
    ~on_error:raise
Exception: Failure "Simulated error".
```
