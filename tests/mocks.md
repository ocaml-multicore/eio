## Setup

```ocaml
# #require "eio_main";;
# #require "eio.mock";;
```

```ocaml
open Eio.Std
let stdin = Eio_mock.Flow.make "stdin"
let stdout = Eio_mock.Flow.make "stdout"
```

## Flows

```ocaml
# Eio_main.run @@ fun _ ->
  Eio_mock.Flow.on_read stdin [
    `Return "chunk1";
    `Return "chunk2";
    `Raise End_of_file
  ];
  Eio.Flow.copy stdin stdout;
  Eio.Flow.close stdin;
  Eio.Flow.shutdown stdout `Send;;
+stdin: read "chunk1"
+stdout: wrote "chunk1"
+stdin: read "chunk2"
+stdout: wrote "chunk2"
+stdin: closed
+stdout: shutdown send
- : unit = ()
```

## Networks

A simple test server:

```ocaml
let echo_server ~net addr =
  Switch.run @@ fun sw ->
  let socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  Eio.Net.accept_sub socket ~sw (fun ~sw flow _addr -> Eio.Flow.copy flow flow)
    ~on_error:(traceln "Error handling connection: %a" Fmt.exn);;
```

The server handles a connection:

```ocaml
# Eio_main.run @@ fun _ ->
  let net = Eio_mock.Net.make "mocknet" in
  let listening_socket = Eio_mock.Net.listening_socket "tcp/80" in
  Eio_mock.Net.on_listen net [`Return listening_socket];
  let connection = Eio_mock.Flow.make "connection" in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 37568) in
  Eio_mock.Net.on_accept listening_socket [`Return (connection, addr)];
  Eio_mock.Flow.on_read connection [`Return "foo"; `Return "bar"];
  echo_server ~net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 80));;
+mocknet: listen on tcp:127.0.0.1:80
+tcp/80: accepted connection from tcp:127.0.0.1:37568
+connection: read "foo"
+connection: wrote "foo"
+connection: read "bar"
+connection: wrote "bar"
+connection: closed
+tcp/80: closed
- : unit = ()
```
