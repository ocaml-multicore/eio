open Eio.Std

module Process = Eio.Process

let process env = Eio.Stdenv.process_mgr env

let status = Alcotest.of_pp Process.pp_status

let std_fds =
  Eio_unix.Fd.[ 0, stdin, `Blocking; 1, stdout, `Blocking; 2, stderr, `Blocking ]

(* Anonymous pipes do not yet work on Windows, so the tests pass files to
   the child instead and read them back afterwards. *)
let with_tmp env name fn =
  let path = Eio.Path.(Eio.Stdenv.cwd env / name) in
  Fun.protect ~finally:(fun () -> try Eio.Path.unlink path with Eio.Io _ -> ()) @@ fun () ->
  fn path

let open_out ~sw path = Eio.Path.open_out ~sw ~create:(`Or_truncate 0o600) path

(* Run [args] with stdout sent to a file and return what it wrote. *)
let run_out ?(name="proc-out.txt") ?cwd ?stdin ?env:child_env env args =
  with_tmp env name @@ fun path ->
  Switch.run (fun sw ->
      Process.run (process env) ?cwd ?stdin ?env:child_env ~stdout:(open_out ~sw path) args
    );
  String.trim (Eio.Path.load path)

let test_exit_status env () =
  Switch.run @@ fun sw ->
  let mgr = process env in
  let ok = Process.spawn ~sw mgr ["cmd"; "/c"; "exit"; "0"] in
  Alcotest.check status "exit 0" (`Exited 0) (Process.await ok);
  let bad = Process.spawn ~sw mgr ["cmd"; "/c"; "exit"; "5"] in
  Alcotest.check status "exit 5" (`Exited 5) (Process.await bad)

let test_stdout env () =
  Alcotest.(check string) "stdout" "hello" (run_out env ["cmd"; "/c"; "echo"; "hello"])

let test_stderr env () =
  with_tmp env "proc-err.txt" @@ fun path ->
  Switch.run (fun sw ->
      Process.run (process env) ~stderr:(open_out ~sw path) ["cmd"; "/c"; "echo"; "iluvcamels"; "1>&2"]
    );
  Alcotest.(check string) "stderr" "iluvcamels" (String.trim (Eio.Path.load path))

let test_stdin env () =
  with_tmp env "proc-in.txt" @@ fun path ->
  Eio.Path.save ~create:(`Or_truncate 0o600) path "hello\r\n";
  let line =
    Eio.Path.with_open_in path @@ fun stdin ->
    run_out env ~stdin ["findstr"; "hello"]
  in
  Alcotest.(check string) "echoed stdin" "hello" line

(* Each child must get its own standard handles, even with many spawned at once. *)
let test_stress env () =
  let spawn_one i =
    let token = Printf.sprintf "tok-%d" i in
    let out = run_out env ~name:(Printf.sprintf "proc-%s.txt" token) ["cmd"; "/c"; "echo"; token] in
    Alcotest.(check string) token token out
  in
  Fiber.List.iter ~max_fibers:8 spawn_one (List.init 200 Fun.id)

(* cmd.exe needs SystemRoot *)
let test_env env () =
  let systemroot = Option.value (Sys.getenv_opt "SystemRoot") ~default:"C:\\Windows" in
  let line =
    run_out env ~env:[| "FOO=bar"; "SystemRoot=" ^ systemroot |] ["cmd"; "/c"; "echo"; "%FOO%"]
  in
  Alcotest.(check string) "env var" "bar" line

let test_cwd env () =
  let cwd = Eio.Stdenv.cwd env in
  let subdir = Eio.Path.(cwd / "proc-cwd-test") in
  Eio.Path.mkdir subdir ~perm:0o700;
  Fun.protect ~finally:(fun () -> Eio.Path.rmdir subdir) @@ fun () ->
  let line = run_out env ~cwd:subdir ["cmd"; "/c"; "cd"] in
  Alcotest.(check string) "child cwd" "proc-cwd-test" (Filename.basename line)

let test_missing_cwd env () =
  Switch.run @@ fun sw ->
  let missing = Eio.Path.(Eio.Stdenv.cwd env / "proc-no-such-dir") in
  match Process.spawn ~sw (process env) ~cwd:missing ["cmd"; "/c"; "exit"; "0"] with
  | _ -> Alcotest.fail "Expected Not_found"
  | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> ()

let test_quoting env () =
  let line = run_out env ["cmd"; "/c"; "echo"; "hello world"] in
  Alcotest.(check string) "quoted arg" "\"hello world\"" line

let test_explicit_executable env () =
  Switch.run @@ fun sw ->
  let child =
    Eio_unix.Process.spawn_unix ~sw (process env) ~executable:"cmd"
      ~fds:std_fds ["ignored-argv0"; "/c"; "exit"; "7"]
  in
  Alcotest.check status "exit 7" (`Exited 7) (Process.await child)

let test_fds_above_2_rejected env () =
  Switch.run @@ fun sw ->
  Alcotest.check_raises "fd 3"
    (Invalid_argument "spawn: only fds 0-2 are supported on Windows (got fd 3)")
    (fun () ->
       ignore (Eio_unix.Process.spawn_unix ~sw (process env) ~executable:"cmd.exe"
                 ~fds:(std_fds @ [3, Eio_unix.Fd.stdin, `Blocking])
                 ["cmd"; "/c"; "exit"; "0"]))

(* An unlisted standard handle is inherited, as on Unix. Our stdout is
   pointed at a file while the child runs to see where its output goes. *)
let test_missing_std_fd_inherited env () =
  with_tmp env "proc-inherit.txt" @@ fun path ->
  let saved = Unix.dup Unix.stdout in
  Fun.protect ~finally:(fun () -> Unix.dup2 saved Unix.stdout; Unix.close saved) (fun () ->
      Switch.run @@ fun sw ->
      Eio.Path.with_open_out ~create:(`Or_truncate 0o600) path (fun out ->
          let fd = Option.get (Eio_unix.Resource.fd_opt out) in
          Eio_unix.Fd.use_exn "dup2" fd (fun fd -> Unix.dup2 fd Unix.stdout)
        );
      let child =
        Eio_unix.Process.spawn_unix ~sw (process env) ~executable:"cmd.exe"
          ~fds:[] ["cmd"; "/c"; "echo"; "inherited"]
      in
      Alcotest.check status "exit 0" (`Exited 0) (Process.await child)
    );
  Alcotest.(check string) "stdout" "inherited" (String.trim (Eio.Path.load path))

let test_terminate env () =
  Switch.run @@ fun sw ->
  let child = Process.spawn ~sw (process env) ["ping"; "-n"; "30"; "127.0.0.1"] in
  Process.signal child Sys.sighup;
  Alcotest.check status "terminated" (`Signaled Sys.sighup) (Process.await child)

let test_await_timeout env () =
  Switch.run @@ fun sw ->
  let child = Process.spawn ~sw (process env) ["ping"; "-n"; "30"; "127.0.0.1"] in
  let clock = Eio.Stdenv.clock env in
  (match Eio.Time.with_timeout_exn clock 0.5 (fun () -> Process.await child) with
   | status -> Alcotest.failf "await should have timed out, got %a" Process.pp_status status
   | exception Eio.Time.Timeout -> ());
  Process.signal child Sys.sigterm;
  Alcotest.check status "terminated after the timeout" (`Signaled Sys.sigterm) (Process.await child)

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
  Alcotest.check status "exit 0" (`Exited 0) (Process.await child);
  Process.signal child Sys.sigkill;
  Alcotest.check status "status unchanged" (`Exited 0) (Process.await child)

let test_stop_on_switch_release env () =
  let t0 = Unix.gettimeofday () in
  Switch.run (fun sw ->
      let _child = Process.spawn ~sw (process env) ["ping"; "-n"; "30"; "127.0.0.1"] in
      ());
  let elapsed = Unix.gettimeofday () -. t0 in
  if elapsed > 20.0 then
    Alcotest.failf "switch release did not stop the child (took %.1fs)" elapsed

(* Cancelling the switch kills the child, and await still reports how it ended. *)
let test_stop_on_cancel env () =
  let child = ref None in
  (try
     Switch.run (fun sw ->
         child := Some (Process.spawn ~sw (process env) ["ping"; "-n"; "30"; "127.0.0.1"]);
         Switch.fail sw Exit)
   with Exit -> ());
  Alcotest.check status "killed" (`Signaled Sys.sigkill) (Process.await (Option.get !child))

let test_spawn_failure env () =
  Switch.run @@ fun sw ->
  match Process.spawn ~sw (process env) ["nonexistent-executable-eio-test"] with
  | _ -> Alcotest.fail "spawn of a nonexistent executable should fail"
  | exception Eio.Io (Process.E (Process.Executable_not_found _), _) -> ()

let tests env = [
  "exit-status", `Quick, test_exit_status env;
  "stdout", `Quick, test_stdout env;
  "stderr", `Quick, test_stderr env;
  "stdin", `Quick, test_stdin env;
  "env", `Quick, test_env env;
  "cwd", `Quick, test_cwd env;
  "quoting", `Quick, test_quoting env;
  "explicit-executable", `Quick, test_explicit_executable env;
  "fds-above-2-rejected", `Quick, test_fds_above_2_rejected env;
  "missing-std-fd-inherited", `Quick, test_missing_std_fd_inherited env;
  "missing-cwd", `Quick, test_missing_cwd env;
  "terminate", `Quick, test_terminate env;
  "await-timeout", `Quick, test_await_timeout env;
  "cwd-escape", `Quick, test_cwd_escape env;
  "env-empty-entry", `Quick, test_env_empty_entry env;
  "signal-after-exit", `Quick, test_signal_after_exit env;
  "stop-on-switch-release", `Quick, test_stop_on_switch_release env;
  "stop-on-cancel", `Quick, test_stop_on_cancel env;
  "spawn-failure", `Quick, test_spawn_failure env;
  "stress", `Slow, test_stress env;
]
