# Setting up the environment

```ocaml
# #require "eio_main";;
```

Creating some useful helper functions

```ocaml
open Eio
open Eio.Std

let spawn ~env ~sw ?cwd cmd args =
  Process.spawn ~sw ?cwd ~stdout:env#stdout ~stdin:env#stdin ~stderr:env#stderr env#process_mgr cmd args

let run fn =
  Eio_main.run @@ fun env ->
  fn (spawn ~env) env
```

Running a program as a subprocess

```ocaml
# run @@ fun spawn env ->
  Switch.run @@ fun sw ->
  let t = spawn ~sw "/usr/bin/echo" [ "echo"; "hello world" ] in
  Process.exit_status t;;
hello world
- : Process.status = Eio.Process.Exited 0
```

Stopping a subprocess works and checking the status waits and reports correctly

```ocaml
# run @@ fun spawn _env ->
  Switch.run @@ fun sw ->
  let t = spawn ~sw "/usr/bin/sleep" [ "sleep"; "10" ] in
  Process.signal t Sys.sigkill;
  Process.exit_status t;;
- : Process.status = Eio.Process.Signaled (-7)
```

A switch will stop a process when it is released.
<!-- Need a better test of this... -->

```ocaml
# run @@ fun spawn env ->
  let proc = ref None in 
  let run () =
    Switch.run @@ fun sw ->
    proc := Some (spawn ~sw "/usr/bin/sleep" [ "sleep"; "10" ])
  in
  run ();
  Process.exit_status (Option.get !proc);;
- : Process.status = Eio.Process.Signaled (-7)
```

Passing in flows allows you to redirect the child process' stdout.

```ocaml
# run @@ fun _spawn env ->
  let process = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let filename = "process-test.txt" in
  let run () =
    Eio.Path.(with_open_out ~create:(`Exclusive 0o600) (fs / filename)) @@ fun stdout ->
    let stdout = (stdout :> Eio.Flow.sink) in
    Switch.run @@ fun sw ->
    let t = Eio.Process.spawn ~sw ~stdout ~stdin:env#stdin ~stderr:env#stderr process "/usr/bin/echo" [ "echo"; "Hello" ] in
    Process.exit_status t
  in
  match run () with
    | Exited 0 ->
      Eio.Path.(with_open_in (fs / filename)) @@ fun flow ->
      let buff = Buffer.create 128 in
      Eio.Flow.copy flow (Eio.Flow.buffer_sink buff);
      Buffer.contents buff
    | _ -> failwith "Subprocess didn't exit cleanly!";;
- : string = "Hello\n"
```

Pipes

```ocaml
# let with_pipe_from_child fn =
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  fn ~sw ~r ~w;;
val with_pipe_from_child :
  (sw:Switch.t ->
   r:< close : unit; probe : 'a. 'a Generic.ty -> 'a option;
       read_into : Cstruct.t -> int; read_methods : Flow.read_method list;
       unix_fd : [ `Peek | `Take ] -> Unix.file_descr > ->
   w:< close : unit; copy : 'b. (#Flow.source as 'b) -> unit;
       probe : 'a. 'a Generic.ty -> 'a option;
       unix_fd : [ `Peek | `Take ] -> Unix.file_descr;
       write : Cstruct.t list -> unit > ->
   'c) ->
  'c = <fun>
# let pread env =
  with_pipe_from_child @@ fun ~sw ~r ~w ->
  let t =
    Eio.Process.spawn ~sw ~stdout:(w :> Flow.sink) ~stdin:env#stdin ~stderr:env#stderr env#process_mgr "/usr/bin/echo" [ "echo"; "Hello" ] 
  in
  let status = Process.exit_status t in
  Eio.traceln "%a" Eio.Process.pp_status status;
  Flow.close w;
  let buff = Buffer.create 10 in
  Flow.copy r (Flow.buffer_sink buff);
  Buffer.contents buff;;
val pread :
  < process_mgr : #Process.mgr; stderr : #Flow.sink; stdin : #Flow.source;
    .. > ->
  string = <fun>
# run @@ fun _spawn env ->
  pread env;;
+Exited 0
- : string = "Hello\n"
```

Spawning subprocesses in new domains works normally

```ocaml
# run @@ fun spawn env ->
  let mgr = Eio.Stdenv.domain_mgr env in
  Eio.Domain_manager.run mgr @@ fun () ->
  Switch.run @@ fun sw ->
  let t = spawn ~sw "/usr/bin/echo" [ "echo"; "Hello from another domain" ] in
  Process.exit_status t;;
Hello from another domain
- : Process.status = Eio.Process.Exited 0
```

Calling `await_exit` multiple times on the same spawn just returns the status.

```ocaml
# run @@ fun spawn env ->
  Switch.run @@ fun sw ->
  let t = spawn ~sw "/usr/bin/echo" [ "echo"; "hello world" ] in
  (Process.exit_status t, Process.exit_status t, Process.exit_status t);;
hello world
- : Process.status * Process.status * Process.status =
(Eio.Process.Exited 0, Eio.Process.Exited 0, Eio.Process.Exited 0)
```

Using sources and sinks that are not backed by file descriptors.

```ocaml
# run @@ fun _spawn env ->
  let proc = env#process_mgr in
  let buf = Buffer.create 16 in
  let dst = Flow.buffer_sink buf in
  Eio.Switch.run @@ fun sw ->
  let p = 
    Eio.Process.spawn proc ~sw ~stdin:env#stdin ~stdout:dst ~stderr:env#stderr "/usr/bin/echo" [ "echo"; "Hello, world" ]
  in
  let _ : Process.status = Process.exit_status p in
  Buffer.contents buf
- : string = "Hello, world\n"
```
