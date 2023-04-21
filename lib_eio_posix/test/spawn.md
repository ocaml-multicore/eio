```ocaml
# #require "eio_posix";;
```

```ocaml
open Eio.Std

module Process = Eio_posix.Low_level.Process
```

## Spawning processes

Setting environment variables:

```ocaml
# Eio_posix.run @@ fun _env ->
  Switch.run @@ fun sw ->
  let child = Process.spawn ~sw Process.Fork_action.[
    execve "/usr/bin/env"
      ~argv:[| "env" |]
      ~env:[| "FOO=bar" |];
  ] in
  Promise.await (Process.exit_status child);;
FOO=bar
- : Unix.process_status = Unix.WEXITED 0
```

Changing directory:

```ocaml
# Eio_posix.run @@ fun _env ->
  Switch.run @@ fun sw ->
  let child = Process.spawn ~sw Process.Fork_action.[
    chdir "/";
    execve "/usr/bin/env"
      ~argv:[| "env"; "pwd" |]
      ~env:(Unix.environment ())
  ] in
  Promise.await (Process.exit_status child);;
/
- : Unix.process_status = Unix.WEXITED 0
```

Changing directory using a file descriptor:

```ocaml
# Eio_posix.run @@ fun _env ->
  Switch.run @@ fun sw ->
  let root = Eio_posix.Low_level.openat ~sw ~mode:0 "/" Eio_posix.Low_level.Open_flags.(rdonly + directory) in
  let child = Process.spawn ~sw Process.Fork_action.[
    fchdir root;
    execve "/usr/bin/env"
      ~argv:[| "env"; "pwd" |]
      ~env:(Unix.environment ())
  ] in
  Promise.await (Process.exit_status child);;
/
- : Unix.process_status = Unix.WEXITED 0
```

Exit status:

```ocaml
# Eio_posix.run @@ fun _env ->
  Switch.run @@ fun sw ->
  let child = Process.spawn ~sw Process.Fork_action.[
    execve "/usr/bin/env"
      ~argv:[| "env"; "false" |]
      ~env:(Unix.environment ())
  ] in
  Promise.await (Process.exit_status child);;
- : Unix.process_status = Unix.WEXITED 1
```

Failure starting child:

```ocaml
# Eio_posix.run @@ fun _env ->
  Switch.run @@ fun sw ->
  try
    let _child =
      Process.spawn ~sw Process.Fork_action.[
        chdir "/idontexist";
        execve "/usr/bin/env"
          ~argv:[| "env"; "pwd" |]
          ~env:(Unix.environment ())
      ]
    in
    assert false
  with Failure ex ->
    String.sub ex 0 7
- : string = "chdir: "
```

Signalling a running child:

```ocaml
# Eio_posix.run @@ fun _env ->
  Switch.run @@ fun sw ->
  let child =
    Process.spawn ~sw Process.Fork_action.[
      execve "/usr/bin/env"
        ~argv:[| "env"; "sleep"; "1000" |]
        ~env:(Unix.environment ())
    ]
  in
  Process.signal child Sys.sigkill;
  match Promise.await (Process.exit_status child) with
  | Unix.WSIGNALED x when x = Sys.sigkill -> traceln "Child got SIGKILL"
  | _ -> assert false;;
+Child got SIGKILL
- : unit = ()
```

Signalling an exited child does nothing:

```ocaml
# Eio_posix.run @@ fun _env ->
  Switch.run @@ fun sw ->
  let child =
    Process.spawn ~sw Process.Fork_action.[
      execve "/usr/bin/env"
        ~argv:[| "env" |]
        ~env:[| "FOO=bar" |];
    ]
  in
  ignore (Promise.await (Process.exit_status child) : Unix.process_status);
  Process.signal child Sys.sigkill;;
FOO=bar
- : unit = ()
```

Inheriting file descriptors:

```ocaml
let fd flow = Eio_unix.Resource.fd flow
let int_of_fd : Unix.file_descr -> int = Obj.magic
let id flow = Eio_unix.Fd.use_exn "id" (fd flow) int_of_fd
let read_all pipe =
  let r = Eio.Buf_read.of_flow pipe ~max_size:1024 in
  Eio.Buf_read.take_all r
```

```ocaml
# Eio_posix.run @@ fun _env ->
  Switch.run @@ fun sw ->
  let pipe_r, pipe_w = Eio_unix.pipe sw in
  let child =
    Process.spawn ~sw Process.Fork_action.[
      inherit_fds [
        1, fd pipe_w, `Blocking;
      ];
      execve "/usr/bin/env"
        ~argv:[| "env" |]
        ~env:[| "FOO=bar" |];
    ]
  in
  Eio.Flow.close pipe_w;
  let r = Eio.Buf_read.of_flow pipe_r ~max_size:1024 in
  traceln "Read: %S" (Eio.Buf_read.take_all r);
  Promise.await (Process.exit_status child);;
+Read: "FOO=bar\n"
- : Unix.process_status = Unix.WEXITED 0
```

Swapping FDs (note: plain sh can't handle multi-digit FDs!):

```ocaml
# Eio_posix.run @@ fun _env ->
  Switch.run @@ fun sw ->
  let pipe1_r, pipe1_w = Eio_unix.pipe sw in
  let pipe2_r, pipe2_w = Eio_unix.pipe sw in
  let pipe3_r, pipe3_w = Eio_unix.pipe sw in
  let pipe4_r, pipe4_w = Eio_unix.pipe sw in
  let child =
    Process.spawn ~sw Process.Fork_action.[
      inherit_fds [
        id pipe1_w, fd pipe2_w, `Blocking;
        id pipe2_w, fd pipe1_w, `Blocking;
        id pipe3_w, fd pipe4_w, `Blocking;
        id pipe4_w, fd pipe3_w, `Blocking;
      ];
      execve "/usr/bin/env"
        ~argv:[|
          "env"; "bash"; "-c";
          Printf.sprintf "echo one >&%d; echo two >&%d; echo three >&%d; echo four >&%d"
            (id pipe1_w)
            (id pipe2_w)
            (id pipe3_w)
            (id pipe4_w)
          |]
        ~env:(Unix.environment ())
    ]
  in
  Eio.Flow.close pipe1_w;
  Eio.Flow.close pipe2_w;
  Eio.Flow.close pipe3_w;
  Eio.Flow.close pipe4_w;
  traceln "Pipe1: %S" (read_all pipe1_r);
  traceln "Pipe2: %S" (read_all pipe2_r);
  traceln "Pipe3: %S" (read_all pipe3_r);
  traceln "Pipe4: %S" (read_all pipe4_r);
  Promise.await (Process.exit_status child);;
+Pipe1: "two\n"
+Pipe2: "one\n"
+Pipe3: "four\n"
+Pipe4: "three\n"
- : Unix.process_status = Unix.WEXITED 0
```

Keeping an FD open:

```ocaml
# Eio_posix.run @@ fun _env ->
  Switch.run @@ fun sw ->
  let pipe1_r, pipe1_w = Eio_unix.pipe sw in
  let child =
    Process.spawn ~sw Process.Fork_action.[
      inherit_fds [
        id pipe1_w, fd pipe1_w, `Blocking;
      ];
      execve "/usr/bin/env"
        ~argv:[| "env"; "bash"; "-c"; Printf.sprintf "echo one >&%d" (id pipe1_w) |]
        ~env:(Unix.environment ())
    ]
  in
  Eio.Flow.close pipe1_w;
  traceln "Pipe1: %S" (read_all pipe1_r);
  Promise.await (Process.exit_status child);;
+Pipe1: "one\n"
- : Unix.process_status = Unix.WEXITED 0
```
