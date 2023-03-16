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
