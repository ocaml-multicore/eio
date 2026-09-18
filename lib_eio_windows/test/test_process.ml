open Eio.Std

module Process = Eio.Process

let process env = Eio.Stdenv.process_mgr env

let read_all flow =
  let b = Buffer.create 100 in
  Eio.Flow.copy flow (Eio.Flow.buffer_sink b);
  Buffer.contents b

let check_status msg expected = function
  | `Exited code when code = expected -> ()
  | status ->
    Alcotest.failf "%s: expected exit %d, got %a" msg expected Process.pp_status status

let check_signaled msg expected = function
  | `Signaled signum when signum = expected -> ()
  | status ->
    Alcotest.failf "%s: expected signal %d, got %a" msg expected Process.pp_status status

let std_fds =
  Eio_unix.Fd.[ 0, stdin, `Blocking; 1, stdout, `Blocking; 2, stderr, `Blocking ]

let test_exit_status env () =
  Switch.run @@ fun sw ->
  let mgr = process env in
  let ok = Process.spawn ~sw mgr ["cmd"; "/c"; "exit"; "0"] in
  check_status "exit 0" 0 (Process.await ok);
  let bad = Process.spawn ~sw mgr ["cmd"; "/c"; "exit"; "5"] in
  check_status "exit 5" 5 (Process.await bad)

let test_stdout_capture env () =
  let line = Process.parse_out (process env) Eio.Buf_read.line ["cmd"; "/c"; "echo"; "hello"] in
  Alcotest.(check string) "stdout" "hello" line

(* A buffer sink is not fd-backed, so a pipe and a copying fiber are needed *)
let test_stdout_flow_copy env () =
  let b = Buffer.create 16 in
  Process.run (process env) ~stdout:(Eio.Flow.buffer_sink b) ["cmd"; "/c"; "echo"; "hello"];
  Alcotest.(check string) "stdout" "hello" (String.trim (Buffer.contents b))

let test_stderr_flow_copy env () =
  let b = Buffer.create 16 in
  Process.run (process env) ~stderr:(Eio.Flow.buffer_sink b) ["cmd"; "/c"; "echo"; "iluvcamels"; "1>&2"];
  Alcotest.(check string) "stderr" "iluvcamels" (String.trim (Buffer.contents b))

let test_stdin_flow_copy env () =
  let line =
    Process.parse_out (process env) Eio.Buf_read.line
      ~stdin:(Eio.Flow.string_source "hello\r\n")
      ["findstr"; "hello"]
  in
  Alcotest.(check string) "echoed stdin" "hello" line

let test_explicit_pipes env () =
  Switch.run @@ fun sw ->
  let from_child, to_parent = Eio_unix.pipe sw in
  let from_parent, to_child = Eio_unix.pipe sw in
  let child = Process.spawn ~sw (process env) ~stdin:from_parent ~stdout:to_parent ["findstr"; "hello"] in
  Eio.Flow.close to_parent;
  Eio.Flow.copy_string "hello\r\n" to_child;
  Eio.Flow.close to_child;
  let out = read_all from_child in
  check_status "findstr" 0 (Process.await child);
  Alcotest.(check string) "roundtrip" "hello" (String.trim out)

(* A handle leaking into a sibling would delay its pipe's EOF *)
let test_stress env () =
  let mgr = process env in
  let spawn_one i =
    Switch.run @@ fun sw ->
    let from_child, to_parent = Eio_unix.pipe sw in
    let token = Printf.sprintf "tok-%d" i in
    let child = Process.spawn ~sw mgr ~stdout:to_parent ["cmd"; "/c"; "echo"; token] in
    Eio.Flow.close to_parent;
    let out = read_all from_child in
    check_status token 0 (Process.await child);
    Alcotest.(check string) token token (String.trim out)
  in
  Fiber.List.iter ~max_fibers:8 spawn_one (List.init 1_000 Fun.id)

let test_no_handle_leak env () =
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  let w_fd = Option.get (Eio_unix.Resource.fd_opt w) in
  Eio_unix.Fd.use_exn "pipe" w_fd Unix.clear_close_on_exec;
  (* TODO: check localhost pinging works on Windows CI *)
  let child = Process.spawn ~sw (process env) ["ping"; "-n"; "30"; "127.0.0.1"] in
  Eio.Flow.close w;
  let result =
    Fiber.first
      (fun () ->
         match Eio.Flow.single_read r (Cstruct.create 1) with
         | _ -> `Data
         | exception End_of_file -> `Eof)
      (fun () -> Eio.Time.sleep (Eio.Stdenv.clock env) 5.0; `Timeout)
  in
  Process.signal child Sys.sigkill;
  ignore (Process.await child : Process.exit_status);
  match result with
  | `Eof -> ()
  | `Data -> Alcotest.fail "unexpected data on pipe"
  | `Timeout -> Alcotest.fail "child inherited the pipe's write handle"

(* cmd.exe needs SystemRoot *)
let test_env env () =
  let systemroot = Option.value (Sys.getenv_opt "SystemRoot") ~default:"C:\\Windows" in
  let line =
    Process.parse_out (process env) Eio.Buf_read.line
      ~env:[| "FOO=bar"; "SystemRoot=" ^ systemroot |]
      ["cmd"; "/c"; "echo"; "%FOO%"]
  in
  Alcotest.(check string) "env var" "bar" line

let test_cwd env () =
  let cwd = Eio.Stdenv.cwd env in
  let subdir = Eio.Path.(cwd / "proc-cwd-test") in
  Eio.Path.mkdir subdir ~perm:0o700;
  Fun.protect ~finally:(fun () -> Eio.Path.rmdir subdir) @@ fun () ->
  let line = Process.parse_out (process env) Eio.Buf_read.line ~cwd:subdir ["cmd"; "/c"; "cd"] in
  Alcotest.(check string) "child cwd" "proc-cwd-test" (Filename.basename line)

let test_missing_cwd env () =
  Switch.run @@ fun sw ->
  let missing = Eio.Path.(Eio.Stdenv.cwd env / "proc-no-such-dir") in
  match Process.spawn ~sw (process env) ~cwd:missing ["cmd"; "/c"; "exit"; "0"] with
  | _ -> Alcotest.fail "Expected Not_found"
  | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> ()

(* Writing more than the pipe holds must not stop other fibers from running,
   so the signal below lands while the write is still blocked. *)
let test_slow_stdin env () =
  Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let r, w = Eio_unix.pipe sw in
  let child = Process.spawn ~sw (process env) ~stdin:r ["ping"; "-n"; "10"; "127.0.0.1"] in
  Eio.Flow.close r;
  Fiber.both
    (fun () ->
       (* Fails once the child, and so its end of the pipe, is gone. *)
       try Eio.Flow.copy_string (String.make (1024 * 1024) 'x') w
       with Eio.Io _ -> ())
    (fun () ->
       Eio.Time.sleep clock 0.1;
       Process.signal child Sys.sigkill);
  check_signaled "killed while the write was blocked" Sys.sigkill (Process.await child)

let test_quoting env () =
  let line = Process.parse_out (process env) Eio.Buf_read.line ["cmd"; "/c"; "echo"; "hello world"] in
  Alcotest.(check string) "quoted arg" "\"hello world\"" line

let test_explicit_executable env () =
  Switch.run @@ fun sw ->
  let child =
    Eio_unix.Process.spawn_unix ~sw (process env) ~executable:"cmd"
      ~fds:std_fds ["ignored-argv0"; "/c"; "exit"; "7"]
  in
  check_status "exit 7" 7 (Process.await child)

let test_fds_above_2_rejected env () =
  Switch.run @@ fun sw ->
  Alcotest.check_raises "fd 3"
    (Invalid_argument "spawn: only fds 0-2 are supported on Windows (got fd 3)")
    (fun () ->
       ignore (Eio_unix.Process.spawn_unix ~sw (process env) ~executable:"cmd.exe"
                 ~fds:(std_fds @ [3, Eio_unix.Fd.stdin, `Blocking])
                 ["cmd"; "/c"; "exit"; "0"]))

(* An unlisted descriptor is inherited, as on Unix. *)
let test_missing_std_fd_inherited env () =
  Switch.run @@ fun sw ->
  let child =
    Eio_unix.Process.spawn_unix ~sw (process env) ~executable:"cmd.exe"
      ~fds:[] ["cmd"; "/c"; "exit"; "3"]
  in
  check_status "inherited" 3 (Process.await child)

let test_terminate env () =
  Switch.run @@ fun sw ->
  let child = Process.spawn ~sw (process env) ["ping"; "-n"; "30"; "127.0.0.1"] in
  Process.signal child Sys.sighup;
  check_signaled "terminated" Sys.sighup (Process.await child)

let test_await_timeout env () =
  Switch.run @@ fun sw ->
  let child = Process.spawn ~sw (process env) ["ping"; "-n"; "30"; "127.0.0.1"] in
  let clock = Eio.Stdenv.clock env in
  (match Eio.Time.with_timeout_exn clock 0.5 (fun () -> Process.await child) with
   | status -> Alcotest.failf "await should have timed out, got %a" Process.pp_status status
   | exception Eio.Time.Timeout -> ());
  Process.signal child Sys.sigterm;
  check_signaled "terminated after the timeout" Sys.sigterm (Process.await child)

let test_cwd_escape env () =
  Switch.run @@ fun sw ->
  let outside = Eio.Path.(Eio.Stdenv.cwd env / "..") in
  match Process.spawn ~sw (process env) ~cwd:outside ["cmd"; "/c"; "cd"] with
  | _ -> Alcotest.fail "cwd outside the sandbox should be refused"
  | exception Eio.Io (Eio.Fs.E (Permission_denied _), _) -> ()

(* An empty entry would end the environment block early. *)
let test_env_empty_entry env () =
  Switch.run @@ fun sw ->
  Alcotest.check_raises "empty entry"
    (Invalid_argument "spawn: invalid environment entry \"\"")
    (fun () ->
       ignore (Eio_unix.Process.spawn_unix ~sw (process env) ~executable:"cmd.exe"
                 ~env:[| "" |] ~fds:std_fds ["cmd"; "/c"; "exit"; "0"]))

let test_signal_after_exit env () =
  Switch.run @@ fun sw ->
  let child = Process.spawn ~sw (process env) ["cmd"; "/c"; "exit"; "0"] in
  check_status "exit 0" 0 (Process.await child);
  Process.signal child Sys.sigkill;
  check_status "status unchanged" 0 (Process.await child)

let test_stop_on_switch_release env () =
  let t0 = Unix.gettimeofday () in
  Switch.run (fun sw ->
      let _child = Process.spawn ~sw (process env) ["ping"; "-n"; "30"; "127.0.0.1"] in
      ());
  let elapsed = Unix.gettimeofday () -. t0 in
  if elapsed > 20.0 then
    Alcotest.failf "switch release did not stop the child (took %.1fs)" elapsed

let test_spawn_failure env () =
  Switch.run @@ fun sw ->
  match Process.spawn ~sw (process env) ["nonexistent-executable-eio-test"] with
  | _ -> Alcotest.fail "spawn of a nonexistent executable should fail"
  | exception Eio.Io (Process.E (Process.Executable_not_found _), _) -> ()

let tests env = [
  "exit-status", `Quick, test_exit_status env;
  "stdout-capture", `Quick, test_stdout_capture env;
  "stdout-flow-copy", `Quick, test_stdout_flow_copy env;
  "stderr-flow-copy", `Quick, test_stderr_flow_copy env;
  "stdin-flow-copy", `Quick, test_stdin_flow_copy env;
  "explicit-pipes", `Quick, test_explicit_pipes env;
  "no-handle-leak", `Quick, test_no_handle_leak env;
  "env", `Quick, test_env env;
  "cwd", `Quick, test_cwd env;
  "quoting", `Quick, test_quoting env;
  "explicit-executable", `Quick, test_explicit_executable env;
  "fds-above-2-rejected", `Quick, test_fds_above_2_rejected env;
  "missing-std-fd-inherited", `Quick, test_missing_std_fd_inherited env;
  "missing-cwd", `Quick, test_missing_cwd env;
  "slow-stdin", `Quick, test_slow_stdin env;
  "terminate", `Quick, test_terminate env;
  "await-timeout", `Quick, test_await_timeout env;
  "cwd-escape", `Quick, test_cwd_escape env;
  "env-empty-entry", `Quick, test_env_empty_entry env;
  "signal-after-exit", `Quick, test_signal_after_exit env;
  "stop-on-switch-release", `Quick, test_stop_on_switch_release env;
  "spawn-failure", `Quick, test_spawn_failure env;
  "stress", `Slow, test_stress env;
]
