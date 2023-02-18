# Set up the test environment

```ocaml
# #require "eio_linux";;
# open Eio.Std;;
# open Eio;;
# module Process = Eio_linux.Low_level.Process;;
module Process = Eio_linux.Low_level.Process
```

A helper function for reading all of the bytes from a handle.

```ocaml
let read_all fd =
  let buf = Cstruct.create 32 in 
  let acc_buffer = Buffer.create 42 in
  let rec read () =
    match Flow.single_read fd buf with
    | i ->
      Buffer.add_string acc_buffer
        Cstruct.(to_string (sub buf 0 i));
      read ()
    | exception End_of_file -> Buffer.contents acc_buffer
  in read ()
```

A simple `echo hello` process redirects to stdout.

```ocaml
# Eio_linux.run @@ fun _env ->
  Switch.run @@ fun sw ->
  let t = Process.spawn ~sw "echo" [ "echo"; "hello" ] in
  Process.await_exit t;;
hello
- : Unix.process_status = Unix.WEXITED 0
```

Using a pipe to redirect output to a buffer.

```ocaml
# Eio_linux.run @@ fun _env ->
  Switch.run @@ fun sw ->
  let rp, wp = Eio_unix.pipe sw in
  let w = Eio_linux.get_fd_opt wp |> Option.get in
  let t = Process.spawn ~sw ~stdout:w "echo" [ "echo"; "Hello,"; "World!" ] in
  let _ = Process.await_exit t in
  Flow.close wp;
  let result = read_all rp in
  result;;
- : string = "Hello, World!\n"
```

Writing to stdin of a process works.

```ocaml
# Eio_linux.run @@ fun _env ->
  Switch.run @@ fun sw ->
  let rp, wp = Eio_unix.pipe sw in
  let r = Eio_linux.get_fd_opt rp |> Option.get in
  let t = Process.spawn ~sw ~stdin:r "head" [ "head" ] in
  Flow.copy_string  "Hello!" wp;
  Flow.close wp;
  Process.await_exit t;;
Hello!
- : Unix.process_status = Unix.WEXITED 0
```

Stopping a process works.

```ocaml
# Eio_linux.run @@ fun _env ->
  Switch.run @@ fun sw ->
  let t = Process.spawn ~sw "sleep" [ "sleep"; "10" ] in
  Process.send_signal t Sys.sigkill;
  Process.await_exit t;;
- : Unix.process_status = Unix.WSIGNALED (-7)
```

Forgetting to wait for a process to finish stops the process.

```ocaml
# Eio_linux.run @@ fun _env ->
  let proc =
    Switch.run @@ fun sw ->
    Process.spawn ~sw "sleep" [ "sleep"; "10" ]
  in
  Process.await_exit proc;;
- : Unix.process_status = Unix.WSIGNALED (-7)
```

Stopping a process interacts nicely with switches.

```ocaml
# Eio_linux.run @@ fun _env ->
  let proc =
    Switch.run @@ fun sw ->
    let t = Process.spawn ~sw "sleep" [ "sleep"; "10" ] in
    Process.send_signal t Sys.sigkill;
    t
  in
  Process.await_exit proc;;
- : Unix.process_status = Unix.WSIGNALED (-7)
```
