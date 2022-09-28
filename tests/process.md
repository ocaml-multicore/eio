# Setting up the environment

```ocaml
# #require "eio_main";;
```

Creating some useful helper functions

```ocaml
open Eio
open Eio.Std

let run (fn : Eio.Process.t -> 'a) =
  Eio_main.run @@ fun env ->
  fn (Eio.Stdenv.process env) env

let run_env (fn : Eio.Stdenv.t -> 'a) =
  Eio_main.run @@ fun env ->
  fn env
```

Running a program as a subprocess

```ocaml
# run @@ fun process env ->
  Switch.run @@ fun sw ->
  let t = Eio.Process.spawn ~sw ~stdout:(env#stdout) process "echo" [ "echo"; "hello world" ] in
  Eio.Process.status t;;
hello world
- : Process.status = Eio.Process.Exited 0
```

Stopping a subprocess works and checking the status waits and reports correctly

```ocaml
# run @@ fun process _env ->
  Switch.run @@ fun sw ->
  let t = Eio.Process.spawn ~sw process "sleep" [ "sleep"; "10" ] in
  Eio.Process.stop t;
  Eio.Process.status t;;
- : Process.status = Eio.Process.Signaled 9
```

A switch will wait for a subprocess to finished when spawned.
<!-- Need a better test of this... -->

```ocaml
# run @@ fun process env ->
  Switch.run @@ fun sw ->
  let _t = Eio.Process.spawn ~sw ~stdout:(env#stdout) process "echo" [ "echo"; "Waited..." ] in
  ();;
Waited...
- : unit = ()
```

Passing in flows allows you to redirect the child process' stdout.

```ocaml
# run_env @@ fun env ->
  let process = Eio.Stdenv.process env in
  let fs = Eio.Stdenv.fs env in
  let filename = "process-test.txt" in
  let run () =
    Eio.Path.(with_open_out ~create:(`Exclusive 0o600) (fs / filename)) @@ fun stdout ->
    let stdout = (stdout :> Eio.Flow.sink) in
    Switch.run @@ fun sw ->
    let t = Eio.Process.spawn ~sw ~stdout process "echo" [ "echo"; "Hello" ] in
    Eio.Process.status t
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
# let pread ~process () =
  with_pipe_from_child @@ fun ~sw ~r ~w ->
  let t =
    Eio.Process.spawn ~sw ~stdout:(w :> Flow.sink) process "echo" [ "echo"; "Hello" ] 
  in
  let status = Eio.Process.status t in
  Eio.traceln "%a" Eio.Process.pp_status status;
  Flow.close w;
  let buff = Buffer.create 10 in
  Flow.copy r (Flow.buffer_sink buff);
  Buffer.contents buff;;
val pread : process:#Process.t -> unit -> string = <fun>
# run @@ fun process _env ->
  pread ~process ();;
+Exited 0
- : string = "Hello\n"
```

Spawning subprocesses in new domains works normally

```ocaml
# run_env @@ fun env ->
  let process = Eio.Stdenv.process env in
  let mgr = Eio.Stdenv.domain_mgr env in
  Eio.Domain_manager.run mgr @@ fun () ->
  Switch.run @@ fun sw ->
  let t = Eio.Process.spawn ~sw ~stdout:(env#stdout) process "echo" [ "echo"; "Hello from another domain" ] in
  Eio.Process.status t;;
Hello from another domain
- : Process.status = Eio.Process.Exited 0
```
