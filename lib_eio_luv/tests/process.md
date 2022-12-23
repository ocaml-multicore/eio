# Set up the test environment

```ocaml
# #require "eio_luv";;
# open Eio.Std;;
# open Eio;;
# module Process = Eio_luv.Low_level.Process;;
module Process = Eio_luv.Low_level.Process
```

A helper function for reading all of the bytes from a handle.

```ocaml
let read_all handle buf =
  let rec read acc =
    match Eio_luv.Low_level.Stream.read_into handle buf with
    | i -> read (acc + i)
    | exception End_of_file -> acc
  in read 0
```

A simple `echo hello` process redirects to stdout.

```ocaml
# Eio_luv.run @@ fun _env ->
  Switch.run @@ fun sw ->
  let redirect = Luv.Process.[
    inherit_fd ~fd:stdout ~from_parent_fd:stdout ()
  ] in
  let t = Process.spawn ~sw ~redirect "echo" [ "echo"; "hello" ] in
  Process.await_exit t;;
hello
- : int * int64 = (0, 0L)
```

Using a pipe to redirect output to a buffer.

```ocaml
# Eio_luv.run @@ fun _env ->
  Switch.run @@ fun sw ->
  let parent_pipe = Eio_luv.Low_level.Pipe.init ~sw () in
  let buf = Luv.Buffer.create 32 in 
  let redirect = Eio_luv.Low_level.Process.[
    to_parent_pipe ~fd:Luv.Process.stdout ~parent_pipe ()
  ] in
  let t = Process.spawn ~sw ~redirect "echo" [ "echo"; "Hello,"; "World!" ] in
  let read = read_all parent_pipe buf in
  let _ = Process.await_exit t in
  Luv.Buffer.to_string (Luv.Buffer.sub buf ~offset:0 ~length:read);;
- : string = "Hello, World!\n"
```

Writing to stdin of a process works.

```ocaml
# Eio_luv.run @@ fun _env ->
  Switch.run @@ fun sw ->
  let parent_pipe = Eio_luv.Low_level.Pipe.init ~sw () in
  let bufs = [ Luv.Buffer.from_string "Hello!" ] in 
  let redirect = Luv.Process.[
    inherit_fd ~fd:stdout ~from_parent_fd:stdout ();
    Process.to_parent_pipe ~fd:stdin ~parent_pipe ()
  ] in
  let t = Process.spawn ~sw ~redirect "head" [ "head" ] in
  Eio_luv.Low_level.Stream.write parent_pipe bufs;
  Eio_luv.Low_level.Handle.close parent_pipe;
  Process.await_exit t;;
Hello!
- : int * int64 = (0, 0L)
```

Stopping a process works.

```ocaml
# Eio_luv.run @@ fun _env ->
  Switch.run @@ fun sw ->
  let redirect = Luv.Process.[
    inherit_fd ~fd:stdout ~from_parent_fd:stdout ()
  ] in
  let t = Process.spawn ~sw ~redirect "sleep" [ "sleep"; "10" ] in
  Process.send_signal t Luv.Signal.sigkill;
  Process.await_exit t;;
- : int * int64 = (9, 0L)
```

Forgetting to wait for a process to finish stops the process.

```ocaml
# Eio_luv.run @@ fun _env ->
  let proc =
    Switch.run @@ fun sw ->
    let redirect = Luv.Process.[
      inherit_fd ~fd:stdout ~from_parent_fd:stdout ()
    ] in
    Process.spawn ~sw ~redirect "sleep" [ "sleep"; "10" ]
  in
  Process.await_exit proc;;
- : int * int64 = (9, 0L)
```

Stopping a process interacts nicely with switches.

```ocaml
# Eio_luv.run @@ fun _env ->
  let proc =
    Switch.run @@ fun sw ->
    let redirect = Luv.Process.[
      inherit_fd ~fd:stdout ~from_parent_fd:stdout ()
    ] in
    let t = Process.spawn ~sw ~redirect "sleep" [ "sleep"; "10" ] in
    Process.send_signal t Luv.Signal.sigkill;
    t
  in
  Process.await_exit proc;;
- : int * int64 = (9, 0L)
```
