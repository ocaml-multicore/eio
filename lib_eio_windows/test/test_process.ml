open Eio.Std

module Process = Eio.Process

let process env = Eio.Stdenv.process_mgr env

let status = Alcotest.of_pp Process.pp_status

let std_fds =
  Eio_unix.Fd.[ 0, stdin, `Blocking; 1, stdout, `Blocking; 2, stderr, `Blocking ]

let with_tmp env name fn =
  let path = Eio.Path.(Eio.Stdenv.cwd env / name) in
  Fun.protect ~finally:(fun () -> Eio.Path.unlink path ~missing_ok:true) @@ fun () ->
  fn path

let open_out ~sw path = Eio.Path.open_out ~sw ~create:(`Or_truncate 0o600) path

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

let test_pipe_write_prefix env () =
  Switch.run @@ fun sw ->
  let r, w = Process.pipe ~sw (process env) in
  let _, n = Fiber.pair
      (fun () -> Eio.Flow.read_exact r (Cstruct.create 1); Eio.Flow.close r)
      (fun () -> Eio.Flow.single_write w [Cstruct.create (1024 * 1024)])
  in
  Alcotest.(check bool) "reported bytes transferred before reader closed" true (n > 0)

let test_cancel_pipe_write env () =
  Switch.run @@ fun sw ->
  let r, w = Process.pipe ~sw (process env) in
  let had_to_close = ref false in
  Fiber.fork_daemon ~sw (fun () ->
      Eio.Time.sleep env#clock 3.;
      had_to_close := true;
      Eio.Flow.close r;
      `Stop_daemon);
  (match Eio.Time.with_timeout_exn env#clock 0.05 (fun () ->
      Eio.Flow.write w [Cstruct.create (1024 * 1024)]) with
   | () -> Alcotest.fail "write completed with no reader"
   | exception Eio.Time.Timeout -> ());
  Alcotest.(check bool) "write cancelled with reader still open" false !had_to_close

let test_socket_stdout env () =
  Switch.run @@ fun sw ->
  let output, peer = Eio_unix.Net.socketpair_stream ~sw () in
  Process.run ~stdout:output (process env) [Sys.executable_name; "list"];
  Eio.Flow.close output;
  let text = Eio.Buf_read.(parse_exn take_all) peer ~max_size:65536 in
  Alcotest.(check bool) "child wrote to the socket" true (text <> "")

let test_shared_stdout env () =
  with_tmp env "proc-shared-stdout.txt" @@ fun path ->
  let data = String.make (1024 * 1024) 'a' in
  Eio.Path.save ~create:(`Or_truncate 0o600) path data;
  Switch.run @@ fun sw ->
  let mgr = process env in
  let reader, stdout = Process.pipe ~sw mgr in
  let writer = Process.spawn ~sw ~stdout mgr ["cmd"; "/c"; "type"; snd path] in
  let fd = Option.get (Eio_unix.Resource.fd_opt reader) in
  Eio_unix.Fd.use_exn "await output" fd Eio_unix.await_readable;
  let other = Switch.run (fun guard ->
      let expired = ref false in
      Fiber.fork_daemon ~sw:guard (fun () ->
          Eio.Time.sleep env#clock 5.;
          expired := true;
          Eio.Flow.close reader;
          `Stop_daemon);
      let other = Process.spawn ~sw ~stdout mgr ["cmd"; "/c"; "exit"; "0"] in
      Alcotest.(check bool) "spawned before draining stdout" false !expired;
      other) in
  Eio.Flow.close stdout;
  let buf = Cstruct.create (String.length data) in
  Eio.Flow.read_exact reader buf;
  Alcotest.check status "writer" (`Exited 0) (Process.await writer);
  Alcotest.check status "other child" (`Exited 0) (Process.await other)

let write_merged_output () =
  assert (input_char stdin = '!');
  assert (Iocp.same_handle Unix.stdout Unix.stderr);
  for _ = 1 to 32 do
    output_char stdout 'A'; flush stdout;
    output_char stderr 'B'; flush stderr
  done

let test_merged_output ~separate env () =
  with_tmp env "proc-merged.txt" @@ fun path ->
  Switch.run (fun sw ->
      let raw = Iocp.openfile (snd path) [O_WRONLY; O_CREAT; O_TRUNC] 0o600 in
      let stderr = Eio_unix.File.import_rw ~sw ~close_unix:true raw in
      let stdout = if separate then Eio_unix.File.import_rw ~sw ~close_unix:false raw else stderr in
      let mgr = process env in
      let stdin, trigger = Process.pipe ~sw mgr in
      let child = Process.spawn mgr ~sw ~stdin ~stdout ~stderr [Sys.executable_name; "--merged-output"] in
      Eio.Flow.close stderr;
      Eio.Flow.copy_string "!" trigger;
      Alcotest.check status "child exited" (`Exited 0) (Process.await child));
  Alcotest.(check string) "child write order" (String.init 64 (fun i -> "AB".[i mod 2])) (Eio.Path.load path)

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

let test_stress env () =
  let spawn_one i =
    let token = Printf.sprintf "tok-%d" i in
    let out = run_out env ~name:(Printf.sprintf "proc-%s.txt" token) ["cmd"; "/c"; "echo"; token] in
    Alcotest.(check string) token token out
  in
  Fiber.List.iter ~max_fibers:8 spawn_one (List.init 200 Fun.id)

let test_env env () =
  let systemroot = Option.value (Sys.getenv_opt "SystemRoot") ~default:"C:\\Windows" in
  let line =
    run_out env ~env:[| "FOO=bar"; "SystemRoot=" ^ systemroot |] ["cmd"; "/c"; "echo"; "%FOO%"]
  in
  Alcotest.(check string) "env var" "bar" line

let test_cwd env () =
  let cd = ["cmd"; "/c"; "cd"] in
  Alcotest.(check string) "env#fs as child cwd"
    (run_out env cd) (run_out env ~cwd:(Eio.Stdenv.fs env) cd);
  let cwd = Eio.Stdenv.cwd env in
  let subdir = Eio.Path.(cwd / "proc-cwd-test") in
  Eio.Path.mkdir subdir ~perm:0o700;
  Fun.protect ~finally:(fun () -> Eio.Path.rmdir subdir) @@ fun () ->
  let line = run_out env ~cwd:subdir cd in
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

(* An unlisted standard handle is inherited, as on POSIX. *)
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
  Process.signal child Sys.sigterm;
  Alcotest.check status "terminated" (`Signaled Sys.sigterm) (Process.await child)

let test_any_signal_terminates env () =
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
  let child =
    Switch.run (fun sw -> Process.spawn ~sw (process env) ["ping"; "-n"; "30"; "127.0.0.1"])
  in
  Alcotest.check status "killed" (`Signaled Sys.sigkill) (Process.await child);
  let elapsed = Unix.gettimeofday () -. t0 in
  if elapsed > 20.0 then
    Alcotest.failf "switch release did not stop the child (took %.1fs)" elapsed

let test_stop_on_cancel env () =
  let child = ref None in
  (try
     Switch.run (fun sw ->
         child := Some (Process.spawn ~sw (process env) ["ping"; "-n"; "30"; "127.0.0.1"]);
         Switch.fail sw Exit)
   with Exit -> ());
  Alcotest.check status "killed" (`Signaled Sys.sigkill) (Process.await (Option.get !child))

let test_cancel_during_spawn env () =
  Switch.run @@ fun sw ->
  let cwd = Eio.Stdenv.fs env in
  let spawn target =
    Process.spawn ~sw:target ~cwd (process env) ["nonexistent-executable-eio-test"]
  in
  (* Resolving cwd suspends the spawning fiber before CreateProcess. *)
  let pending = Switch.run (fun target -> Fiber.fork_promise ~sw (fun () -> spawn target)) in
  Alcotest.check_raises "finished switch" (Invalid_argument "Switch finished!")
    (fun () -> ignore (Promise.await_exn pending));
  let context, set_context = Promise.create () in
  let pending = Fiber.fork_promise ~sw (fun () ->
      Eio.Cancel.sub (fun cc -> Promise.resolve set_context cc; spawn sw)) in
  Eio.Cancel.cancel (Promise.await context) Exit;
  Alcotest.check_raises "cancelled caller" (Eio.Cancel.Cancelled Exit)
    (fun () -> ignore (Promise.await_exn pending))

let test_switch_during_spawn env () =
  Switch.run @@ fun sw ->
  let input, _ = Process.pipe ~sw (process env) in
  let fd = Option.get (Eio_unix.Resource.fd_opt input) in
  let launch target =
    Fiber.fork_promise ~sw (fun () ->
        (* Classifying a raw pipe suspends before CreateProcess. *)
        Eio_unix.Process.spawn_unix ~sw:target (process env) ~fds:[0, fd, `Blocking]
          ~executable:"nonexistent-executable-eio-test" ["unused"])
  in
  let pending = Switch.run launch in
  Alcotest.check_raises "finished switch" (Invalid_argument "Switch finished!")
    (fun () -> ignore (Promise.await_exn pending));
  try Switch.run (fun target ->
      let pending = launch target in
      Switch.fail target Exit;
      Eio.Cancel.protect (fun () ->
          Alcotest.check_raises "cancelling switch" (Eio.Cancel.Cancelled Exit)
            (fun () -> ignore (Promise.await_exn pending))))
  with Exit -> ()

let test_spawn_failure env () =
  Switch.run @@ fun sw ->
  match Process.spawn ~sw (process env) ["nonexistent-executable-eio-test"] with
  | _ -> Alcotest.fail "spawn of a nonexistent executable should fail"
  | exception Eio.Io (Process.E (Process.Executable_not_found _), _) -> ()

let test_argument_list_too_long env () =
  Switch.run @@ fun sw ->
  List.iter (fun arg ->
      match Process.spawn ~sw (process env) ["cmd"; "/c"; "exit"; "0"; arg] with
      | _ -> Alcotest.fail "oversized command line should fail"
      | exception Eio.Io (Process.E Process.Argument_list_too_long, _) -> ()
    ) [
      String.make 32768 'x';
      String.concat "" (List.init 17000 (Fun.const "\u{1f600}"));
    ]

let tests () = [
  "exit-status", `Quick, test_exit_status;
  "stdout", `Quick, test_stdout;
  "socket-stdout", `Quick, test_socket_stdout;
  "shared-stdout", `Quick, test_shared_stdout;
  "merged-output", `Quick, test_merged_output ~separate:false;
  "merged-output-wrappers", `Quick, test_merged_output ~separate:true;
  "cancel-pipe-write", `Quick, test_cancel_pipe_write;
  "pipe-write-prefix", `Quick, test_pipe_write_prefix;
  "stderr", `Quick, test_stderr;
  "stdin", `Quick, test_stdin;
  "env", `Quick, test_env;
  "cwd", `Quick, test_cwd;
  "quoting", `Quick, test_quoting;
  "explicit-executable", `Quick, test_explicit_executable;
  "fds-above-2-rejected", `Quick, test_fds_above_2_rejected;
  "missing-std-fd-inherited", `Quick, test_missing_std_fd_inherited;
  "missing-cwd", `Quick, test_missing_cwd;
  "terminate", `Quick, test_terminate;
  "any-signal-terminates", `Quick, test_any_signal_terminates;
  "await-timeout", `Quick, test_await_timeout;
  "cwd-escape", `Quick, test_cwd_escape;
  "env-empty-entry", `Quick, test_env_empty_entry;
  "signal-after-exit", `Quick, test_signal_after_exit;
  "stop-on-switch-release", `Quick, test_stop_on_switch_release;
  "stop-on-cancel", `Quick, test_stop_on_cancel;
  "cancel-during-spawn", `Quick, test_cancel_during_spawn;
  "switch-during-spawn", `Quick, test_switch_during_spawn;
  "spawn-failure", `Quick, test_spawn_failure;
  "argument-list-too-long", `Quick, test_argument_list_too_long;
  "stress", `Slow, test_stress;
]
