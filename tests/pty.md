# Setting up the environment

```ocaml
# #require "eio_main";;
```

```ocaml
open Eio.Std

module Pty = Eio_unix.Pty

let () = Eio.Exn.Backend.show := false

let run fn =
  Eio_main.run @@ fun env ->
  fn env
```

## Opening a pseudoterminal

A new pair has a terminal-device name and two distinct file descriptors:

```ocaml
# run @@ fun _env ->
  Switch.run @@ fun sw ->
  let t = Pty.open_pty ~sw () in
  String.starts_with ~prefix:"/dev/" (Pty.name t);;
- : bool = true
```

## Window size

The window size can be set on either end and read back:

```ocaml
# run @@ fun _env ->
  Switch.run @@ fun sw ->
  let t = Pty.open_pty ~sw () in
  Pty.set_window_size (Pty.pty t) { rows = 24; cols = 80; xpixel = 0; ypixel = 0 };
  Pty.get_window_size (Pty.tty t);;
- : Pty.winsize = {Pty.rows = 24; cols = 80; xpixel = 0; ypixel = 0}
```

## Terminal attributes

Terminal attributes can be manipulted via `Pty.Tc`. Here we change a setting
through the draining path, drain and flush the queues, resume output, and read
the new value back. This exercises some of the fiber blocking logic:

```ocaml
# run @@ fun _env ->
  Switch.run @@ fun sw ->
  let tty = Pty.tty (Pty.open_pty ~sw ()) in
  let attr = Pty.Tc.getattr tty in
  Pty.Tc.setattr tty Unix.TCSADRAIN { attr with c_echo = false };
  Pty.Tc.drain tty;
  Pty.Tc.flush tty Unix.TCIOFLUSH;
  Pty.Tc.flow tty Unix.TCOON;
  (Pty.Tc.getattr tty).c_echo;;
- : bool = false
```

## Spawning a child on a pseudoterminal

Passing the terminal-device end as `login_tty` makes it the child's controlling
terminal and stdin/stdout/stderr, so the child sees all three as a tty:

```ocaml
# run @@ fun env ->
  Switch.run @@ fun sw ->
  let t = Pty.open_pty ~sw () in
  let child =
    Eio_unix.Process.spawn_unix ~sw env#process_mgr
      ~login_tty:(Pty.tty t)
      ~fds:[]
      ["sh"; "-c"; "[ -t 0 ] && [ -t 1 ] && [ -t 2 ]"]
  in
  Eio.Process.await child;;
- : Eio.Process.exit_status = `Exited 0
```

Output the child writes to its terminal can be read back from the `pty` end.
The child ends with a blocking `read` so that it keeps the terminal end open until
we have consumed its output, since some BSD-derived systems discard output
as soon as the last terminal-side fd is closed. Linux seems to preserve the
output however. The trailing `read` is released when the switch exits.

```ocaml
# run @@ fun env ->
  Switch.run @@ fun sw ->
  let t = Pty.open_pty ~sw () in
  let _child =
    Eio_unix.Process.spawn_unix ~sw env#process_mgr
      ~login_tty:(Pty.tty t)
      ~fds:[]
      ["sh"; "-c"; "for i in 1 2 3; do echo \"line $i\"; done; read keep_open"]
  in
  let r = Eio.Buf_read.of_flow (Pty.source t) ~max_size:1024 in
  Eio.Buf_read.lines r |> Seq.take 3 |> List.of_seq;;
- : string list = ["line 1"; "line 2"; "line 3"]
```

Input can be sent to the child by writing to the `pty` end with {!Pty.sink}.
Here we turn off the terminal's input echo, send it a line, and read the response:

```ocaml
# run @@ fun env ->
  Switch.run @@ fun sw ->
  let t = Pty.open_pty ~sw () in
  let attr = Pty.Tc.getattr (Pty.tty t) in
  Pty.Tc.setattr (Pty.tty t) Unix.TCSANOW { attr with c_echo = false };
  let _child =
    Eio_unix.Process.spawn_unix ~sw env#process_mgr
      ~login_tty:(Pty.tty t)
      ~fds:[]
      ["sh"; "-c"; "read line; echo \"reply: $line\"; read keep_open"]
  in
  Eio.Flow.copy_string "hello\n" (Pty.sink t);
  let r = Eio.Buf_read.of_flow (Pty.source t) ~max_size:1024 in
  Eio.Buf_read.line r;;
- : string = "reply: hello"
```

With echo left on, the terminal echoes the input back to the `pty` end as well:

```ocaml
# run @@ fun env ->
  Switch.run @@ fun sw ->
  let t = Pty.open_pty ~sw () in
  let _child =
    Eio_unix.Process.spawn_unix ~sw env#process_mgr
      ~login_tty:(Pty.tty t)
      ~fds:[]
      ["sh"; "-c"; "read line; echo \"reply: $line\"; read keep_open"]
  in
  Eio.Flow.copy_string "hello\n" (Pty.sink t);
  let r = Eio.Buf_read.of_flow (Pty.source t) ~max_size:1024 in
  Eio.Buf_read.lines r |> Seq.take 2 |> List.of_seq;;
- : string list = ["hello"; "reply: hello"]
```

Without a controlling terminal, the same check fails:

```ocaml
# run @@ fun env ->
  Switch.run @@ fun sw ->
  let devnull = Eio_unix.Fd.of_unix ~sw ~blocking:true ~close_unix:true
      (Unix.openfile "/dev/null" [O_RDWR] 0) in
  let child =
    Eio_unix.Process.spawn_unix ~sw env#process_mgr
      ~fds:[ 0, devnull, `Blocking; 1, devnull, `Blocking; 2, devnull, `Blocking ]
      ["sh"; "-c"; "[ -t 0 ]"]
  in
  Eio.Process.await child;;
- : Eio.Process.exit_status = `Exited 1
```

Detect if the user tries to set the standard streams manually:

```ocaml
# run @@ fun env ->
  Switch.run @@ fun sw ->
  let t = Pty.open_pty ~sw () in
  let devnull = Eio_unix.Fd.of_unix ~sw ~blocking:true ~close_unix:true
      (Unix.openfile "/dev/null" [O_RDWR] 0) in
  Eio_unix.Process.spawn_unix ~sw env#process_mgr
    ~login_tty:(Pty.tty t)
    ~fds:[ 0, devnull, `Blocking ]
    ["sh"; "-c"; "exit 0"];;
Exception: Invalid_argument "FD 0 assigned twice!".
```
