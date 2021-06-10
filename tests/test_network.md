# Setting up the environment

```ocaml
# #require "eunix";;
```

```ocaml
open Eio.Std

let run (fn : network:Eio.Network.t -> Switch.t -> unit) =
  try
    Eunix.run @@ fun env ->
    let network = Eio.Stdenv.network env in
    Switch.top (fn ~network);
    print_endline "ok"
  with
  | Failure msg -> print_endline msg
  | ex -> print_endline (Printexc.to_string ex)

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
let run_client ~sw ~network ~addr =
  traceln "Connecting to server...";
  let flow = Eio.Network.connect ~sw network addr in
  Eio.Flow.copy_string "Hello from client" flow;
  Eio.Flow.shutdown flow `Send;
  let msg = read_all ~sw flow in
  traceln "Client received: %S" msg
```

A simple server:

```ocaml
let run_server ~sw socket =
  while true do
    Eio.Network.Listening_socket.accept_sub socket ~sw (fun ~sw flow _addr ->
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
```

Handling one connection, then cancelling the server:

```ocaml
# run @@ fun ~network sw ->
  let server = Eio.Network.bind network ~sw ~reuse_addr:true addr in
  Eio.Network.Listening_socket.listen server 5;
  Fibre.both ~sw
    (fun () -> run_server ~sw server)
    (fun () ->
      run_client ~sw ~network ~addr;
      traceln "Client finished - cancelling server";
      Switch.turn_off sw (Failure "Test is over")
    )
Connecting to server...
Server accepted connection from client
Server received: "Hello from client"
Client received: "Bye"
Client finished - cancelling server
Test is over
- : unit = ()
```

Cancelling the read:

```ocaml
# run @@ fun ~network sw ->
  Switch.top @@ fun read_switch ->
  let server = Eio.Network.bind network ~sw ~reuse_addr:true addr in
  Eio.Network.Listening_socket.listen server 5;
  Fibre.both ~sw
    (fun () ->
      Eio.Network.Listening_socket.accept_sub server ~sw (fun ~sw flow _addr ->
        try
          let msg = read_all ~sw:read_switch flow in
          traceln "Server received: %S" msg
        with Switch.Cancelled Graceful_shutdown ->
          Eio.Flow.copy_string "Request cancelled" flow
      ) ~on_error:raise
    )
    (fun () ->
      traceln "Connecting to server...";
      let flow = Eio.Network.connect ~sw network addr in
      traceln "Connection opened - cancelling server's read";
      Switch.turn_off read_switch Graceful_shutdown;
      let msg = read_all flow in
      traceln "Client received: %S" msg
    )
Connecting to server...
Connection opened - cancelling server's read
Client received: "Request cancelled"
Graceful_shutdown
- : unit = ()
```

Calling accept when the switch is already off:

```ocaml
# run @@ fun ~network sw ->
  let server = Eio.Network.bind network ~sw ~reuse_addr:true addr in
  Eio.Network.Listening_socket.listen server 5;
  Switch.turn_off sw (Failure "Simulated error");
  Eio.Network.Listening_socket.accept_sub server ~sw (fun ~sw:_ _flow _addr -> assert false)
    ~on_error:raise
Simulated error
- : unit = ()
```
