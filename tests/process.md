# Setting up the environment

```ocaml
# #require "eio_main";;
```

Creating some useful helper functions

```ocaml
open Eio.Std

module Flow = Eio.Flow
module Process = Eio.Process

let () = Eio.Exn.Backend.show := false

let ( / ) = Eio.Path.( / )

let run ?clear:(paths = []) fn =
  Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  List.iter (fun p -> Eio.Path.rmtree ~missing_ok:true (cwd / p)) paths;
  fn env#process_mgr env

let status_to_string = Fmt.to_to_string Eio.Process.pp_status
```

Running a program as a subprocess:

```ocaml
# run @@ fun mgr _env ->
  Switch.run @@ fun sw ->
  let t = Process.spawn ~sw mgr [ "echo"; "hello world" ] in
  Process.await t;;
hello world
- : Process.exit_status = `Exited 0
```

Stopping a subprocess works and checking the status waits and reports correctly:

```ocaml
# run @@ fun mgr _env ->
  Switch.run @@ fun sw ->
  let t = Process.spawn ~sw mgr [ "sleep"; "10" ] in
  Process.signal t Sys.sigkill;
  Process.await t |> status_to_string
- : string = "Exited (signal SIGKILL)"
```

A switch will stop a process when it is released:

```ocaml
# run @@ fun mgr env ->
  let proc = Switch.run (fun sw -> Process.spawn ~sw mgr [ "sleep"; "10" ]) in
  Process.await proc |> status_to_string
- : string = "Exited (signal SIGKILL)"
```

Passing in flows allows you to redirect the child process' stdout:

```ocaml
# run ~clear:["process-test.txt"] @@ fun mgr env ->
  let fs = Eio.Stdenv.fs env in
  let path = fs / "process-test.txt" in
  Eio.Path.(with_open_out ~create:(`Exclusive 0o600) path) @@ fun stdout ->
  Process.run mgr ~stdout [ "echo"; "Hello" ];
  Eio.Path.(load path);;
- : string = "Hello\n"
```

Piping data to and from the child:

```ocaml
# run @@ fun mgr env ->
  let stdin = Eio.Flow.string_source "one\ntwo\nthree\n" in
  Process.parse_out mgr Eio.Buf_read.line ~stdin ["wc"; "-l"] |> String.trim;;
- : string = "3"
```

Spawning subprocesses in new domains works normally:

```ocaml
# run @@ fun mgr env ->
  Eio.Domain_manager.run env#domain_mgr @@ fun () ->
  Process.run mgr [ "echo"; "Hello from another domain" ];;
Hello from another domain
- : unit = ()
```

Calling `await_exit` multiple times on the same spawn just returns the status:

```ocaml
# run @@ fun mgr env ->
  Switch.run @@ fun sw ->
  let t = Process.spawn ~sw mgr [ "echo"; "hello world" ] in
  (Process.await t, Process.await t, Process.await t);;
hello world
- : Process.exit_status * Process.exit_status * Process.exit_status =
(`Exited 0, `Exited 0, `Exited 0)
```

Using a sink that is not backed by a file descriptor:

```ocaml
# run @@ fun mgr env ->
  let buf = Buffer.create 16 in
  Eio.Process.run mgr ~stdout:(Flow.buffer_sink buf) [ "echo"; "Hello, world" ];
  Buffer.contents buf
- : string = "Hello, world\n"
```

Changing directory (unconfined):

```ocaml
# run @@ fun mgr env ->
  let root = env#fs / "/" in
  Process.run mgr ~cwd:root [ "env"; "pwd" ];;
/
- : unit = ()
```

Changing directory (confined):

```ocaml
# run ~clear:["proc-sub-dir"] @@ fun mgr env ->
  let cwd = Eio.Stdenv.cwd env in
  let subdir = cwd / "proc-sub-dir" in
  Eio.Path.mkdir subdir ~perm:0o700;
  Eio.Path.with_open_dir subdir @@ fun subdir ->
  Eio.Path.save (subdir / "test-cwd") "test-data" ~create:(`Exclusive 0o600);
  Process.run mgr ~cwd:subdir [ "cat"; "test-cwd" ];;
test-data
- : unit = ()
```

Trying to access a path outside of the cwd:

```ocaml
# run @@ fun mgr env ->
  Process.run mgr ~cwd:(env#cwd / "..") [ "cat"; "test-cwd" ];;
Exception: Eio.Io Fs Permission_denied _
```

If a command fails, we get shown the arguments (quoted if necessary):

```ocaml
# run @@ fun mgr env ->
  Process.run mgr ["sh"; "-c"; "exit 3"; ""; "foo"; "\"bar\""];;
Exception:
Eio.Io Process Child_error Exited (code 3),
  running command: sh -c "exit 3" "" foo "\"bar\""
```

Exit code success can be determined by is_success (Process.run):

```ocaml
# run @@ fun mgr env ->
  Process.run ~is_success:(Int.equal 3) mgr ["sh"; "-c"; "exit 3"];;
- : unit = ()

# run @@ fun mgr env ->
  Process.run ~is_success:(Int.equal 3) mgr ["sh"; "-c"; "exit 0"];;
Exception:
Eio.Io Process Child_error Exited (code 0),
  running command: sh -c "exit 0"
```

Exit code success can be determined by is_success (Process.parse_out):

```ocaml
# run @@ fun mgr env ->
  Process.parse_out ~is_success:(Int.equal 5) mgr Eio.Buf_read.line ["sh"; "-c"; "echo 123; exit 5"];;
- : string = "123"
```

The default environment:

```ocaml
# run @@ fun mgr env ->
  Unix.putenv "DISPLAY" ":1";
  Process.parse_out mgr Eio.Buf_read.line ["sh"; "-c"; "echo $DISPLAY"];;
- : string = ":1"
```

A custom environment:

```ocaml
# run @@ fun mgr env ->
  let env = [| "DISPLAY=:2" |] in
  Process.parse_out mgr Eio.Buf_read.line ["sh"; "-c"; "echo $DISPLAY"] ~env;;
- : string = ":2"
```

Eio's child reaping code doesn't interfere with OCaml's process spawning:

```ocaml
let rec waitpid_with_retry flags pid =
  try Unix.waitpid flags pid
  with Unix.Unix_error(Unix.EINTR, _, _) -> waitpid_with_retry flags pid
```

```ocaml
# Eio_main.run @@ fun env ->
  let p = Unix.(create_process "/usr/bin/env" [|"env"; "echo"; "hi"|] stdin stdout stderr) in
  Eio.Time.Mono.sleep env#mono_clock 0.01;
  waitpid_with_retry [] p |> snd;;
hi
- : Unix.process_status = Unix.WEXITED 0
```
