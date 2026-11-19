# Setting up the environment

```ocaml
# #require "eio_main";;
# #install_printer Eio.Process.Env.pp;;
```

Creating some useful helper functions

```ocaml
open Eio.Std

module Flow = Eio.Flow
module Process = Eio.Process
module Env = Process.Env

let () = Eio.Exn.Backend.show := false

let ( / ) = Eio.Path.( / )

let run ?clear:(paths = []) fn =
  Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  List.iter (fun p -> Eio.Path.rmtree ~missing_ok:true (cwd / p)) paths;
  fn env#process_mgr env

let command args = "./process_helper.exe" :: args

let status_to_string = Fmt.to_to_string Eio.Process.pp_status
```

On Windows, override the process helpers to run [process_helper.ml](process_helper.ml)
for the commands used below. Other platforms use the real commands.

<!-- $MDX os_type=Win32 -->
```ocaml
module Process = struct
  include Eio.Process

  let helper = Filename.concat (Sys.getcwd ()) "process_helper.exe"

  let command executable args =
    match executable, args with
    | None, ("echo" | "sleep" | "env" | "cat" | "sh" | "wc") :: _ ->
      Some helper, "./process_helper.exe" :: args
    | _ -> executable, args

  let spawn ~sw mgr ?cwd ?stdin ?stdout ?stderr ?env ?executable args =
    let executable, args = command executable args in
    spawn ~sw mgr ?cwd ?stdin ?stdout ?stderr ?env ?executable args

  let run mgr ?cwd ?stdin ?stdout ?stderr ?is_success ?env ?executable args =
    let executable, args = command executable args in
    run mgr ?cwd ?stdin ?stdout ?stderr ?is_success ?env ?executable args

  let parse_out mgr parser ?cwd ?stdin ?stderr ?is_success ?env ?executable args =
    let executable, args = command executable args in
    parse_out mgr parser ?cwd ?stdin ?stderr ?is_success ?env ?executable args
end
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

Arguments are passed literally, including spaces, quotes, and shell metacharacters:

```ocaml
# run @@ fun mgr _env ->
  Eio.Process.run mgr (command ["args"; ""; "two words"; "\"quoted\""; "space and trailing\\"; "&|<>^%!"]);;
""
"two words"
"\"quoted\""
"space and trailing\\"
"&|<>^%!"
- : unit = ()
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
# if Sys.win32 then mdx_skip "Anonymous pipes are not supported yet on Windows (eio#929)";
  run @@ fun mgr env ->
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
# if Sys.win32 then mdx_skip "Anonymous pipes are not supported yet on Windows (eio#929)";
  run @@ fun mgr env ->
  let buf = Buffer.create 16 in
  Process.run mgr ~stdout:(Flow.buffer_sink buf) [ "echo"; "Hello, world" ];
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
  Eio.Path.with_subtree subdir @@ fun subdir ->
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

<!-- $MDX os_type<>Win32 -->
```ocaml
# run @@ fun mgr env ->
  Process.run mgr ["sh"; "-c"; "exit 3"; ""; "foo"; "\"bar\""];;
Exception:
Eio.Io Process Child_error Exited (code 3),
  running command: sh -c "exit 3" "" foo "\"bar\""
```

<!-- $MDX os_type=Win32 -->
```ocaml
# run @@ fun mgr env ->
  Process.run mgr ["sh"; "-c"; "exit 3"; ""; "foo"; "\"bar\""];;
Exception:
Eio.Io Process Child_error Exited (code 3),
  running command: ./process_helper.exe sh -c "exit 3" "" foo "\"bar\""
```

Passing too many arguments reports a dedicated error rather than a generic failure:

```ocaml
# run @@ fun mgr _env ->
  let big = String.make (1024 * 1024) 'x' in
  Process.run mgr ("echo" :: List.init 8 (fun _ -> big));;
Exception: Eio.Io Process Argument_list_too_long
```

Windows limits the command line to 32,767 UTF-16 code units, including its terminator:

```ocaml
# if not Sys.win32 then mdx_skip "Windows command-line size limit";
  run @@ fun mgr _env ->
  let invoke arg = Eio.Process.run mgr (command ["exit"; "0"; arg]) in
  let too_long arg =
    match invoke arg with
    | () -> assert false
    | exception Eio.Io (Process.E Process.Argument_list_too_long, _) -> ()
  in
  let prefix = String.concat " " (command ["exit"; "0"]) ^ " " in
  let max_arg = 32766 - String.length prefix in
  invoke (String.make max_arg 'x');
  too_long (String.make (max_arg + 1) 'x');
  let emoji n = String.concat "" (List.init n (Fun.const "\u{1f600}")) in
  invoke (emoji 12000);
  too_long (emoji 17000);;
- : unit = ()
```

Failures relating to the executable itself also report dedicated errrors.
A file that exists but isn't executable:

```ocaml
# if Sys.win32 then mdx_skip "Windows does not use POSIX executable permission bits";
  run ~clear:["ocamlrocks"] @@ fun mgr env ->
  let f = Eio.Stdenv.cwd env / "ocamlrocks" in
  Eio.Path.save f "#!/bin/sh\necho hi\n" ~create:(`Exclusive 0o600);
  Fun.protect ~finally:(fun () -> Eio.Path.unlink f) (fun () -> Process.run mgr ["./ocamlrocks"]);;
Exception: Eio.Io Process Permission_denied "./ocamlrocks"
```

A file that is executable but not in a runnable format:

```ocaml
# run ~clear:["badfmt"] @@ fun mgr env ->
  let f = Eio.Stdenv.cwd env / "badfmt" in
  Eio.Path.save f "\000\001 not a binary \255" ~create:(`Exclusive 0o700);
  Fun.protect ~finally:(fun () -> Eio.Path.unlink f) (fun () -> Process.run mgr ["./badfmt"]);;
Exception: Eio.Io Process Executable_format_error "./badfmt"
```

A script whose interpreter is missing reports the executable as not found:

```ocaml
# if Sys.win32 then mdx_skip "Windows does not interpret POSIX shebangs";
  run ~clear:["badinterp"] @@ fun mgr env ->
  let f = Eio.Stdenv.cwd env / "badinterp" in
  Eio.Path.save f "#!/nonexistent/interpreter\n" ~create:(`Exclusive 0o700);
  Fun.protect ~finally:(fun () -> Eio.Path.unlink f) (fun () -> Process.run mgr ["./badinterp"]);;
Exception: Eio.Io Process Executable_not_found "./badinterp"
```

Exit code success can be determined by is_success (Process.run):

<!-- $MDX os_type<>Win32 -->
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

<!-- $MDX os_type=Win32 -->
```ocaml
# run @@ fun mgr env ->
  Process.run ~is_success:(Int.equal 3) mgr ["sh"; "-c"; "exit 3"];;
- : unit = ()

# run @@ fun mgr env ->
  Process.run ~is_success:(Int.equal 3) mgr ["sh"; "-c"; "exit 0"];;
Exception:
Eio.Io Process Child_error Exited (code 0),
  running command: ./process_helper.exe sh -c "exit 0"
```

Exit code success can be determined by is_success (Process.parse_out):

```ocaml
# if Sys.win32 then mdx_skip "Anonymous pipes are not supported yet on Windows (eio#929)";
  run @@ fun mgr env ->
  Process.parse_out ~is_success:(Int.equal 5) mgr Eio.Buf_read.line ["sh"; "-c"; "echo 123; exit 5"];;
- : string = "123"
```

The default environment:

```ocaml
# if Sys.win32 then mdx_skip "Anonymous pipes are not supported yet on Windows (eio#929)";
  run @@ fun mgr env ->
  Unix.putenv "DISPLAY" ":1";
  Process.parse_out mgr Eio.Buf_read.line ["sh"; "-c"; "echo $DISPLAY"];;
- : string = ":1"
```

<!-- $MDX os_type=Win32 -->
```ocaml
# run @@ fun mgr env ->
  Unix.putenv "DISPLAY" ":1";
  Process.run mgr ["sh"; "-c"; "echo $DISPLAY"];;
:1
- : unit = ()
```

A custom environment:

```ocaml
# if Sys.win32 then mdx_skip "Anonymous pipes are not supported yet on Windows (eio#929)";
  run @@ fun mgr env ->
  let env = Env.of_bindings ["DISPLAY", ":2"] in
  Process.parse_out mgr Eio.Buf_read.line ["sh"; "-c"; "echo $DISPLAY"] ~env;;
- : string = ":2"
```

<!-- $MDX os_type=Win32 -->
```ocaml
# run @@ fun mgr env ->
  let env = Env.of_bindings ["DISPLAY", ":2"] in
  Process.run mgr ["sh"; "-c"; "echo $DISPLAY"] ~env;;
:2
- : unit = ()
```

Using the parent's environment explicitly:

```ocaml
# if Sys.win32 then mdx_skip "Anonymous pipes are not supported yet on Windows (eio#929)";
  run @@ fun mgr env ->
  Unix.putenv "DISPLAY" ":1";
  let env = Eio.Process.environment mgr in
  Process.parse_out ~env mgr Eio.Buf_read.line ["sh"; "-c"; "echo $DISPLAY"];;
- : string = ":1"
```

<!-- $MDX os_type=Win32 -->
```ocaml
# run @@ fun mgr env ->
  Unix.putenv "DISPLAY" ":1";
  let env = Eio.Process.environment mgr in
  Process.run ~env mgr ["sh"; "-c"; "echo $DISPLAY"];;
:1
- : unit = ()
```

Eio's child reaping code doesn't interfere with OCaml's process spawning:

```ocaml
let rec waitpid_with_retry flags pid =
  try Unix.waitpid flags pid
  with Unix.Unix_error(Unix.EINTR, _, _) -> waitpid_with_retry flags pid
```

```ocaml
# Eio_main.run @@ fun env ->
  let executable, args =
    if Sys.win32 then "./process_helper.exe", [|"./process_helper.exe"; "echo"; "hi"|]
    else "/usr/bin/env", [|"env"; "echo"; "hi"|]
  in
  let p = Unix.(create_process executable args stdin stdout stderr) in
  Eio.Time.Mono.sleep env#mono_clock 0.01;
  waitpid_with_retry [] p |> snd;;
hi
- : Unix.process_status = Unix.WEXITED 0
```

Manipulating environments:

```ocaml
# Env.empty;;
- : Env.t = []

# let e = Env.of_array [| "HOME=/home/user"; "malformed"; "DISPLAY=:0"; "EMPTY=" |];;
val e : Env.t = ["HOME=/home/user"
                 "malformed"
                 "DISPLAY=:0"
                 "EMPTY="]

# e |> Env.get_opt "DISPLAY";;
- : string option = Some ":0"
# e |> Env.get_opt "missing";;
- : string option = None
# e |> Env.get_opt "EMPTY";;
- : string option = Some ""
# e |> Env.get_opt "malformed";;
- : string option = None

# e |> Env.override [
    "HOME", Some "/home/bob";
    "DISPLAY", None;
    "MISSING", None;
    "EXTRA", Some "a";
    "EXTRA", Some "b";
    "X", Some "x";
    "X", None;
  ];;
- : Env.t = ["HOME=/home/bob"
             "malformed"
             "EMPTY="
             "EXTRA=b"]

# try Env.of_bindings ["k=", "v"] |> ignore
  with Invalid_argument x -> print_endline x;;
Invalid environment variable name "k="
- : unit = ()

# e |> Env.override ["k", Some "v=1"] |> Env.get_opt "k";;
- : string option = Some "v=1"

# Env.(get_opt "" (of_array [| "=foo" |]));;
- : string option = None

# try e |> Env.override ["", None] |> ignore
  with Invalid_argument x -> print_endline x;;
Invalid environment variable name ""
- : unit = ()

# let e = Env.of_array [| ""; "a=1"; "a=2"; "b=3"; "b=4"; "c=5"; "c=6" |];;
val e : Env.t = [""
                 "a=1"
                 "a=2"
                 "b=3"
                 "b=4"
                 "c=5"
                 "c=6"]
# e |> Env.override [
    "a", Some "7";
    "b", None;
  ];;
- : Env.t = [""
             "a=7"
             "c=5"
             "c=6"]
```

Using the environment capability:

```ocaml
# run @@ fun mgr _env ->
  Unix.putenv "DISPLAY" ":1";
  Eio.Process.getenv_opt mgr "DISPLAY";;
- : string option = Some ":1"
```

```ocaml
# run @@ fun mgr _env ->
  Eio.Process.getenv_opt mgr "THIS_VAR_PROBABLY_WILL_NOT_EXIST";;
- : string option = None
```

```ocaml
# run @@ fun mgr _env ->
  Unix.putenv "DISPLAY" ":1";
  let env = Eio.Process.environment mgr in
  Eio.Process.Env.get_opt "DISPLAY" env;;
- : string option = Some ":1"
```
