(* Copyright (C) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: ISC *)

open Eio.Std

let cancel_read env () =
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  for _ = 1 to 50 do
    Fiber.first
      (fun () -> ignore (Eio.Flow.single_read r (Cstruct.create 1)); Alcotest.fail "unexpected data")
      (fun () -> Fiber.yield ())
  done;
  Gc.full_major ();
  Eio.Time.with_timeout_exn env#clock 2.0 (fun () ->
      Fiber.both
        (fun () -> Eio.Flow.copy_string "x" w)
        (fun () ->
           let b = Cstruct.create 1 in
           Eio.Flow.read_exact r b;
           Alcotest.(check string) "pipe remains usable" "x" (Cstruct.to_string b)))

let positioned env () =
  let path = Eio.Path.(env#cwd / "iocp-positioned") in
  Eio.Path.save path "0123456789" ~create:(`Exclusive 0o600);
  Fun.protect ~finally:(fun () -> Eio.Path.unlink path) @@ fun () ->
  Eio.Path.with_open_in path @@ fun f ->
  let at = Optint.Int63.of_int in
  ignore (Eio.File.seek f (at 3) `Set);
  let buf = Cstruct.create 3 in
  Eio.File.pread_exact f ~file_offset:(at 6) [buf];
  Alcotest.(check string) "positioned data" "678" (Cstruct.to_string buf);
  Alcotest.(check int) "cursor unchanged" 3 (Optint.Int63.to_int (Eio.File.seek f (at 0) `Cur));
  let fd = Option.get (Eio_unix.Resource.fd_opt f) in
  List.iter (fun io ->
      List.iter (fun bufs ->
          match io ~file_offset:(at (-1)) fd bufs with
          | _ -> Alcotest.fail "accepted a negative file offset"
          | exception Invalid_argument _ -> ()) [ [||]; [|buf|] ]
    ) [Eio_windows.Low_level.preadv; Eio_windows.Low_level.pwritev];
  (match Eio_windows.Low_level.pwritev fd ~file_offset:(at 0) [|Cstruct.of_string "BAD"|] with
   | _ -> Alcotest.fail "write gained authority from read-only handle"
   | exception Unix.Unix_error (EBADF, _, _) -> ());
  Effect.Deep.try_with (Eio.Flow.read_exact f) buf {
    effc = fun (type a) (eff : a Effect.t) -> match eff with
      | Eio_unix.Private.Await_readable _ -> Alcotest.fail "regular file polled for readiness"
      | _ -> None
  };
  Alcotest.(check string) "sequential data" "345" (Cstruct.to_string buf)

let process_capture env () =
  let got = Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all ["cmd"; "/c"; "echo hello"] in
  Alcotest.(check string) "captured output" "hello" (String.trim got)

let cancel_write env () =
  Switch.run @@ fun sw ->
  let _r, w = Eio_unix.pipe sw in
  Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
      Fiber.first
        (fun () -> Eio.Flow.write w [Cstruct.create (1024 * 1024)]; Alcotest.fail "write should block")
        (fun () -> Fiber.yield (); Gc.full_major ()))

let pipe_eof env () =
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
      Fiber.both
        (fun () ->
           match Eio.Flow.single_read r (Cstruct.create 1) with
           | _ -> Alcotest.fail "expected pipe EOF"
           | exception End_of_file -> ())
        (fun () -> Fiber.yield (); Eio.Flow.close w))

let cross_domain_pipe env () =
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
      Fiber.both
        (fun () -> Eio.Domain_manager.run env#domain_mgr (fun () -> Eio.Flow.copy_string "cross-domain" w))
        (fun () ->
           let buf = Cstruct.create 12 in
           Eio.Flow.read_exact r buf;
           Alcotest.(check string) "domain pipe" "cross-domain" (Cstruct.to_string buf)))

let first_completion_in_worker env () =
  Eio.Domain_manager.run env#domain_mgr (fun () ->
      Switch.run @@ fun sw ->
      let r, w = Eio_unix.pipe sw in
      Fiber.both
        (fun () -> Eio.Flow.copy_string "x" w)
        (fun () -> Eio.Flow.read_exact r (Cstruct.create 1)))

let register_on_fresh_port raw =
  let port = Iocp.create () in
  Fun.protect ~finally:(fun () -> Iocp.close port) (fun () ->
      Iocp.close_handle (Iocp.register port raw))

let remove_during_read env () =
  Switch.run @@ fun sw ->
  let r, _w = Eio_unix.pipe sw in
  let fd = Eio_unix.Resource.fd r in
  let cancelled, cancel = Promise.create () in
  Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
      Fiber.both
        (fun () -> Fiber.first
             (fun () -> ignore (Eio.Flow.single_read r (Cstruct.create 1)); Alcotest.fail "unexpected read")
             (fun () -> Promise.await cancelled))
        (fun () ->
           let transfer = Fiber.fork_promise ~sw (fun () -> Eio_unix.Fd.remove fd) in
           Alcotest.(check bool) "remove waits for read" false (Promise.is_resolved transfer);
           Promise.resolve cancel ();
           let raw = Option.get (Promise.await_exn transfer) in
           Fun.protect ~finally:(fun () -> Unix.close raw) (fun () -> register_on_fresh_port raw)))

let positioned_errors env () =
  let path = Eio.Path.(env#cwd / "iocp-positioned-public") in
  Eio.Path.with_open_out ~create:(`Exclusive 0o600) path (fun f ->
      Eio.File.pwrite_all f ~file_offset:Optint.Int63.zero [Cstruct.of_string "abc"]);
  Fun.protect ~finally:(fun () -> Eio.Path.unlink path) @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let fd = Unix.openfile "iocp-positioned-public" [O_RDONLY; O_CLOEXEC] 0 in
  let file = Eio_unix.File.import_rw ~sw ~close_unix:true fd in
  let buf = Cstruct.create 3 in
  Eio.File.pread_exact file ~file_offset:Optint.Int63.zero [buf];
  Alcotest.(check string) "positioned write and read" "abc" (Cstruct.to_string buf);
  match Eio.File.pwrite_all file ~file_offset:Optint.Int63.zero [buf] with
  | () -> Alcotest.fail "wrote to a read-only descriptor"
  | exception Eio.Io (_, _) -> ()

let mixed_waits env () =
  let r, w = Unix.socketpair ~cloexec:true PF_UNIX SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close r; Unix.close w) @@ fun () ->
  Switch.run @@ fun sw ->
  let input, output = Eio_unix.pipe sw in
  Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
      Fiber.both
        (fun () -> Eio_unix.await_readable r)
        (fun () ->
           Fiber.both
             (fun () -> Eio.Flow.read_exact input (Cstruct.create 1))
             (fun () -> Eio.Flow.copy_string "x" output);
           ignore (Unix.single_write_substring w "!" 0 1));
      ignore (Unix.read r (Bytes.create 1) 0 1);
      for _ = 1 to 20 do
        Fiber.first (fun () -> Eio_unix.await_readable r)
          (fun () -> Eio_unix.run_in_systhread (fun () -> ()));
        Eio_unix.run_in_systhread (fun () -> ())
      done)

let sync_read env () =
  Switch.run @@ fun sw ->
  let raw, writer = Unix.pipe ~cloexec:true () in
  let r = Eio_unix.File.import_ro ~sw ~close_unix:true raw in
  (* A delayed writer also bounds the test if a read ignores cancellation. *)
  let writer = Thread.create (fun () ->
      Unix.sleepf 0.4;
      Fun.protect ~finally:(fun () -> Unix.close writer)
        (fun () -> ignore (Unix.single_write writer (Bytes.of_string "x") 0 1))) () in
  Fun.protect ~finally:(fun () -> Thread.join writer) @@ fun () ->
  let buf = Cstruct.create 1 in
  (match Eio.Time.with_timeout_exn env#clock 0.02 (fun () -> Eio.Flow.single_read r buf) with
   | _ -> Alcotest.fail "idle read completed"
   | exception Eio.Time.Timeout -> ());
  Eio.Flow.read_exact r buf;
  Alcotest.(check string) "cancelled read left data for the next reader" "x" (Cstruct.to_string buf)

let sync_pipe_alias () =
  Switch.run @@ fun sw ->
  let r, w = Unix.pipe ~cloexec:true () in
  Fun.protect ~finally:(fun () -> Unix.close w) @@ fun () ->
  let import r = Eio_unix.File.import_ro ~sw ~close_unix:true r in
  let first = import r and second = import (Unix.dup ~cloexec:true r) in
  let waits = ref 0 in
  let read flow =
    let buf = Cstruct.create 1 in
    Effect.Deep.try_with (Eio.Flow.read_exact flow) buf {
      effc = fun (type a) (eff : a Effect.t) -> match eff with
        | Eio_unix.Private.Await_readable _ -> incr waits; None
        | _ -> None
    };
    Cstruct.to_string buf
  in
  let result = Fiber.fork_promise ~sw (fun () -> read first) in
  Fiber.first (fun () -> ignore (read second)) (fun () -> ());
  ignore (Unix.single_write w (Bytes.of_string "x") 0 1);
  Alcotest.(check string) "first read" "x" (Promise.await_exn result);
  Alcotest.(check int) "only the active reader checked readiness" 1 !waits

let child_stdio env () =
  Switch.run @@ fun sw ->
  let stdin, _writer = Eio_unix.pipe sw in
  let run stdin =
    Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
        Eio.Process.parse_out ~stdin env#process_mgr Eio.Buf_read.take_all ["cmd"; "/d"; "/c"; "echo done"])
    |> String.trim |> Alcotest.(check string) "complete child output" "done"
  in
  run (stdin :> Eio.Flow.source_ty r);
  run (Eio.Flow.string_source (String.make (1024 * 1024) 'x'))

let write_prefix () =
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  Fiber.both
    (fun () ->
       let byte = Cstruct.create 1 in
       Eio.Flow.read_exact r byte;
       Alcotest.(check string) "prefix" "X" (Cstruct.to_string byte);
       Eio.Flow.close r)
    (fun () ->
       let n = Eio.Flow.single_write w [Cstruct.empty; Cstruct.of_string "X"; Cstruct.create (1024 * 1024)] in
       Alcotest.(check int) "reported bytes" 1 n)

let completed_write () =
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  let written = ref 0 in
  Fiber.first
    (fun () -> written := Eio.Flow.single_write w [Cstruct.of_string "x"])
    (fun () -> let buf = Cstruct.create 1 in Eio.Flow.read_exact r buf);
  Alcotest.(check int) "reported the byte consumed by the reader" 1 !written

let shared_file_cursor () =
  let path = Filename.temp_file "eio-iocp-cursor" ".tmp" in
  Fun.protect ~finally:(fun () -> Sys.remove path) @@ fun () ->
  Switch.run @@ fun sw ->
  let raw = Iocp.openfile path [Unix.O_RDWR; O_CLOEXEC] 0 in
  let import fd = Eio_unix.File.import_rw ~sw ~close_unix:true fd in
  let a = import raw and b = import (Unix.dup ~cloexec:true raw) in
  Fiber.both (fun () -> Eio.Flow.copy_string "x" a) (fun () -> Eio.Flow.copy_string "y" b);
  Alcotest.(check int) "cursor after both writes" 2 (Optint.Int63.to_int (Eio.File.seek b Optint.Int63.zero `Cur));
  ignore (Eio.File.seek a Optint.Int63.zero `Set);
  let byte f =
    let buf = Cstruct.create 1 in
    Eio.Flow.read_exact f buf;
    Cstruct.to_string buf
  in
  let x, y = Fiber.pair (fun () -> byte a) (fun () -> byte b) in
  Alcotest.(check (list string)) "one byte per alias" ["x"; "y"] (List.sort String.compare [x; y])

let close_during_readiness env () =
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.Net.socketpair_stream ~sw () in
  Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
      let ready = Fiber.fork_promise ~sw (fun () ->
          Eio_windows.Low_level.await_readable (Eio_unix.Resource.fd r)) in
      Eio.Flow.close r;
      Eio_windows.Low_level.await_writable (Eio_unix.Resource.fd w);
      Eio.Flow.copy_string "x" w;
      Promise.await_exn ready)

external handle_count : unit -> int = "caml_eio_test_handle_count"

let failed_stdio env () =
  Switch.run @@ fun sw ->
  let _, stdout = Eio_unix.pipe sw in
  Eio.Flow.close stdout;
  let attempt () =
    match Eio.Process.spawn ~sw ~stdin:(Eio.Flow.string_source "x") ~stdout env#process_mgr ["cmd"] with
    | _ -> Alcotest.fail "spawn accepted a closed output"
    | exception Invalid_argument _ -> ()
  in
  attempt ();
  let before = handle_count () in
  for _ = 1 to 8 do attempt () done;
  Alcotest.(check bool) "temporary pipe handles released" true (handle_count () <= before)

let process_pipe env () =
  Switch.run @@ fun sw ->
  let r, w = Eio.Process.pipe ~sw env#process_mgr in
  let fd f = Option.get (Eio_unix.Resource.fd_opt f) in
  let spawn n f executable args =
    Eio_unix.Process.spawn_unix ~sw env#process_mgr ~fds:[n, fd f, `Blocking] ~executable (executable :: args)
  in
  let consumer = spawn 0 r "findstr.exe" ["/x"; "hello"] in
  let producer = spawn 1 w "cmd.exe" ["/d"; "/c"; "echo hello"] in
  Eio.Flow.close r;
  Eio.Flow.close w;
  Eio.Time.with_timeout_exn env#clock 5.0 (fun () -> List.iter (fun p -> Eio.Process.await_exn p) [producer; consumer])

let duplicated_pipe_readers env () =
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  let readers = List.init 8 (fun _ ->
      let fd = Eio_unix.Fd.use_exn "dup" (Eio_unix.Resource.fd r) (Unix.dup ~cloexec:true) in
      Eio_unix.Fd.of_unix ~sw ~close_unix:true fd, Promise.create ())
  in
  let read (fd, (_, ready)) =
    Eio.Domain_manager.run env#domain_mgr (fun () ->
        Fiber.both
          (fun () ->
             let buf = Cstruct.create 1 in
             for _ = 1 to 128 do
               assert (Eio_windows.Low_level.read_cstruct fd buf = 1);
               assert (Cstruct.get_char buf 0 = 'x')
             done)
          (fun () -> Promise.resolve ready ()))
  in
  Eio.Time.with_timeout_exn env#clock 30. (fun () ->
      Fiber.both
        (fun () -> Fiber.List.iter read readers)
        (fun () ->
           List.iter (fun (_, (started, _)) -> Promise.await started) readers;
           Eio.Flow.copy_string (String.make (List.length readers * 128) 'x') w))

let tests () = List.map (fun (name, fn) -> name, `Quick, fn) [
    "close during low-level readiness wait", close_during_readiness;
    "backend identifier", (fun env () -> Alcotest.(check string) "backend" "windows" env#backend_id);
    "failed stdio preparation releases handles", failed_stdio;
    "direct pipe between child processes", process_pipe;
    "duplicated files share the cursor", (fun _ -> shared_file_cursor);
    "first completion in worker domain", first_completion_in_worker;
    "cancel pipe write", cancel_write;
    "positioned file errors", positioned_errors;
    "switch between readiness and port waits", mixed_waits;
    "synchronous pipe read cancellation", sync_read;
    "duplicated synchronous pipe readers", (fun _ -> sync_pipe_alias);
    "child exit releases stdin and drains stdout", child_stdio;
    "single write reports its prefix", (fun _ -> write_prefix);
    "completed write racing cancellation", (fun _ -> completed_write);
    "pipe EOF", pipe_eof;
    "pipe across domains", cross_domain_pipe;
    "duplicated pipe readers across domains", duplicated_pipe_readers;
    "cancel pipe read", cancel_read;
    "positioned IO preserves cursor and rights", positioned;
    "process output capture", process_capture;
    "remove during a cancelled pipe read", remove_during_read;
  ]
