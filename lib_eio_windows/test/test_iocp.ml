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

let refused env () =
  Switch.run @@ fun sw ->
  let raw = Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close raw) @@ fun () ->
  Unix.bind raw (ADDR_INET (Unix.inet_addr_loopback, 0));
  let port = match Unix.getsockname raw with ADDR_INET (_, p) -> p | _ -> assert false in
  let client = Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close client) (fun () ->
      Unix.set_nonblock client;
      match Unix.connect client (Unix.getsockname raw) with
      | () -> Alcotest.fail "connected to non-listener"
      | exception Unix.Unix_error (ECONNREFUSED, _, _) -> ()
      | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINPROGRESS), _, _) ->
        Eio.Time.with_timeout_exn env#clock 5.0 (fun () -> Eio_unix.await_writable client);
        Alcotest.(check bool) "pending connect error" true (Option.is_some (Unix.getsockopt_error client)));
  match Eio.Time.with_timeout_exn env#clock 10.0 (fun () ->
      Eio.Net.connect ~sw env#net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))) with
  | _ -> Alcotest.fail "connected to non-listener"
  | exception Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) -> ()

let shutdown () =
  Switch.run @@ fun sw ->
  let a, _b = Eio_unix.Net.socketpair_stream ~sw () in
  Fiber.both
    (fun () ->
       match Eio.Flow.single_read a (Cstruct.create 1) with
       | _ -> Alcotest.fail "expected EOF"
       | exception End_of_file -> ())
    (fun () -> Fiber.yield (); Eio.Flow.shutdown a `Receive)

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

let udp env () =
  Switch.run @@ fun sw ->
  let socket () =
    let raw = Unix.socket ~cloexec:true PF_INET SOCK_DGRAM 0 in
    Unix.bind raw (ADDR_INET (Unix.inet_addr_loopback, 0));
    Eio_unix.Net.import_socket_datagram ~sw ~close_unix:true raw,
    Eio_unix.Net.sockaddr_of_unix_datagram (Unix.getsockname raw)
  in
  let a, addr_a = socket () and b, addr_b = socket () in
  Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
      Fiber.first
        (fun () -> ignore (Eio.Net.recv a (Cstruct.create 10)); Alcotest.fail "unexpected datagram")
        (fun () -> Fiber.yield ());
      List.iter (fun msg ->
          Fiber.both
            (fun () -> Eio.Net.send b ~dst:addr_a [Cstruct.of_string msg])
            (fun () ->
               let buf = Cstruct.create 10 in
               let src, n = Eio.Net.recv a buf in
               Alcotest.(check bool) "sender address" true (src = addr_b);
               Alcotest.(check string) "message" msg (Cstruct.to_string (Cstruct.sub buf 0 n)))
        ) ["hello"; ""];
      Eio.Net.send b ~dst:addr_a [];
      let received = ref None in
      Fiber.first
        (fun () -> received := Some (Eio.Net.recv a (Cstruct.create 1)))
        (fun () -> ());
      Alcotest.(check bool) "completed empty datagram survived cancellation"
        true (!received = Some (addr_b, 0));
      let guarded = Cstruct.of_string ".abc." in
      Eio.Net.send b ~dst:addr_a [Cstruct.sub guarded 1 1; Cstruct.sub guarded 2 2];
      let dst = Cstruct.of_string "----" in
      let _, n = Eio.Net.recv a (Cstruct.sub dst 1 2) in
      Alcotest.(check int) "sliced datagram length" 2 n;
      Alcotest.(check string) "receive touches only the supplied slice" "-ab-" (Cstruct.to_string dst);
      Eio.Net.send b ~dst:addr_a [Cstruct.of_string "abc"];
      Eio.Net.send b ~dst:addr_a [Cstruct.of_string "z"];
      let buf = Cstruct.create 1 in
      List.iter (fun expected ->
          let src, n = Eio.Net.recv a buf in
          Alcotest.(check bool) "sender of datagram" true (src = addr_b);
          Alcotest.(check int) "received bytes" 1 n;
          Alcotest.(check string) "datagram prefix" expected (Cstruct.to_string buf))
        ["a"; "z"])

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

let tcp_after_domain_exit env () =
  Switch.run @@ fun sw ->
  let listener = Eio.Net.listen ~sw ~backlog:1 env#net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)) in
  Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
      let client, server = Fiber.pair
          (fun () -> Eio.Net.connect ~sw env#net (Eio.Net.listening_addr listener))
          (fun () -> fst (Eio.Net.accept ~sw listener))
      in
      Eio.Domain_manager.run env#domain_mgr (fun () -> Eio.Flow.copy_string "a" server);
      Eio.Flow.copy_string "b" server;
      let buf = Cstruct.create 2 in
      Eio.Flow.read_exact client buf;
      Alcotest.(check string) "after first writer exited" "ab" (Cstruct.to_string buf);
      Fiber.both
        (fun () -> Eio.Flow.read_exact server buf)
        (fun () -> Eio.Flow.copy_string "cd" client);
      Alcotest.(check string) "queued read after owner exited" "cd" (Cstruct.to_string buf))

let idle_socket_alias env () =
  let module Low_level = Eio_windows.Low_level in
  Switch.run @@ fun sw ->
  let listener = Eio.Net.listen ~sw ~backlog:1 env#net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)) in
  let addr = Eio_unix.Net.sockaddr_to_unix (Eio.Net.listening_addr listener) in
  let check ~dup_before_connect =
    let client = Low_level.socket ~sw Unix.PF_INET Unix.SOCK_STREAM 0 in
    let dup () =
      Eio_unix.Fd.of_unix ~sw ~close_unix:true (Eio_unix.Fd.use_exn "dup" client (Unix.dup ~cloexec:true))
    in
    (match Low_level.shutdown client Unix.SHUTDOWN_SEND with
     | () -> Alcotest.fail "shut down an unconnected socket"
     | exception Unix.Unix_error (ENOTCONN, _, _) -> ());
    let alias = if dup_before_connect then Some (dup ()) else None in
    Low_level.connect client addr;
    let peer, _ = Eio.Net.accept ~sw listener in
    let alias = match alias with Some fd -> fd | None -> dup () in
    Low_level.shutdown alias Unix.SHUTDOWN_SEND;
    match Eio.Flow.single_read peer (Cstruct.create 1) with
    | _ -> Alcotest.fail "expected EOF after shutdown through idle duplicate"
    | exception End_of_file -> ()
  in
  Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
      check ~dup_before_connect:false;
      check ~dup_before_connect:true)

let shared_socket env () =
  Switch.run @@ fun sw ->
  let a, b = Eio_unix.Net.socketpair_stream ~sw () in
  let raw = Eio_unix.Resource.fd a in
  let alias = Eio_unix.Fd.use_exn "dup" raw (Unix.dup ~cloexec:true) in
  let other = Eio_unix.Net.import_socket_stream ~sw ~close_unix:true alias in
  let read flow =
    match Eio.Flow.single_read flow (Cstruct.create 1) with
    | _ -> Alcotest.fail "expected EOF"
    | exception End_of_file -> ()
  in
  Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
      Eio.Flow.copy_string "x" a;
      Eio.Flow.read_exact b (Cstruct.create 1);
      let waiting, ready = Promise.create () in
      Fiber.both
        (fun () -> Eio.Domain_manager.run env#domain_mgr (fun () ->
             Fiber.both (fun () -> read other) (fun () -> Promise.resolve ready ())))
        (fun () -> Promise.await waiting; Eio.Flow.shutdown a `Receive))

let shared_listener env () =
  Switch.run @@ fun sw ->
  let listener = Eio.Net.listen ~sw ~backlog:2 env#net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)) in
  let addr = Eio.Net.listening_addr listener in
  let started, ready = Promise.create () in
  let accept () = ignore (Eio.Net.accept ~sw listener) in
  Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
      Fiber.both
        (fun () -> Eio.Domain_manager.run env#domain_mgr (fun () ->
             Fiber.both accept (fun () -> Promise.resolve ready ())))
        (fun () ->
           Promise.await started;
           Fiber.both accept (fun () ->
               for _ = 1 to 2 do Eio.Flow.close (Eio.Net.connect ~sw env#net addr) done)))

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

let registration_release env () =
  let raw_r, raw_w = Unix.socketpair ~cloexec:true PF_UNIX SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close raw_r; Unix.close raw_w) @@ fun () ->
  for _ = 1 to 3 do
    Switch.run @@ fun sw ->
    let r = Eio_unix.Net.import_socket_stream ~sw ~close_unix:false raw_r in
    let w = Eio_unix.Net.import_socket_stream ~sw ~close_unix:false raw_w in
    Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
        Fiber.both
          (fun () -> Eio.Flow.copy_string "x" w)
          (fun () -> Eio.Flow.read_exact r (Cstruct.create 1)));
    let fd = Eio_unix.Resource.fd r in
    Eio_unix.Fd.use_exn "register idle socket" fd register_on_fresh_port;
    assert (Eio_unix.Fd.remove fd <> None);
    Eio.Flow.close w
  done

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

let local_shutdown env () =
  Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:1 env#net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)) in
  let connect () =
    let c = Eio.Net.connect ~sw env#net (Eio.Net.listening_addr l) in
    ignore (Eio.Net.accept ~sw l);
    c
  in
  let c = connect () in
  let empty_write () = Eio.Flow.single_write c [Cstruct.empty] in
  Alcotest.(check int) "empty write on a connected socket" 0 (empty_write ());
  Eio.Flow.shutdown c `Send;
  (match empty_write () with
   | _ -> Alcotest.fail "empty write after send shutdown"
   | exception Eio.Io (Eio.Net.E (Connection_reset _), _) -> ());
  (match Eio.Flow.copy_string "x" c with
   | () -> Alcotest.fail "write after send shutdown"
   | exception Eio.Io (Eio.Net.E (Connection_reset _), _) -> ());
  let c = connect () in
  Eio.Flow.shutdown c `Receive;
  Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
      match Eio.Flow.single_read c (Cstruct.create 1) with
      | _ -> Alcotest.fail "read after receive shutdown"
      | exception End_of_file -> ())

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

let udp_reset env () =
  Switch.run @@ fun sw ->
  let socket sw = Eio.Net.datagram_socket ~sw env#net (`Udp (Eio.Net.Ipaddr.V4.loopback, 0)) in
  let address s = Eio_unix.Fd.use_exn "getsockname" (Option.get (Eio_unix.Resource.fd_opt s))
      (fun fd -> Eio_unix.Net.sockaddr_of_unix_datagram (Unix.getsockname fd)) in
  let r = socket sw and w = socket sw in
  let closed = Switch.run (fun sw -> address (socket sw)) in
  Eio.Net.send r ~dst:closed [Cstruct.of_string "probe"];
  Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
      Fiber.both
        (fun () ->
           let buf = Cstruct.create 1 in
           let src, n = Eio.Net.recv r buf in
           Alcotest.(check bool) "sender" true (src = address w);
           Alcotest.(check string) "received after ICMP" "x" (Cstruct.to_string (Cstruct.sub buf 0 n)))
        (fun () -> Eio.Time.sleep env#clock 0.1; Eio.Net.send w ~dst:(address r) [Cstruct.of_string "x"]))

let udp_shutdown env () =
  Switch.run @@ fun sw ->
  let socket () = Eio.Net.datagram_socket ~sw env#net (`Udp (Eio.Net.Ipaddr.V4.loopback, 0)) in
  let r = socket () and w = socket () in
  let addr s = Eio_unix.Fd.use_exn "getsockname" (Option.get (Eio_unix.Resource.fd_opt s))
      (fun fd -> Eio_unix.Net.sockaddr_of_unix_datagram (Unix.getsockname fd)) in
  let send msg = Eio.Net.send w ~dst:(addr r) [Cstruct.of_string msg] in
  let recv expected =
    let buf = Cstruct.create 16 in
    let src, n = Eio.Net.recv r buf in
    Alcotest.(check bool) "sender" true (src = addr w);
    Alcotest.(check string) "payload" expected (Cstruct.to_string (Cstruct.sub buf 0 n))
  in
  let Eio.Resource.T (t, ops) = r in
  let module S = (val Eio.Resource.get ops Eio.Flow.Pi.Shutdown) in
  send "queued";
  S.shutdown t `Receive;
  Eio.Time.with_timeout_exn env#clock 5.0 @@ fun () ->
  recv "queued";
  Fiber.both (fun () -> recv "") (fun () -> S.shutdown t `All; send "");
  match Eio.Net.send r ~dst:(addr w) [Cstruct.of_string "x"] with
  | () -> Alcotest.fail "send after datagram send shutdown"
  | exception Eio.Io (Eio.Net.E (Connection_reset _), _) -> ()

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

let shared_readiness env () =
  let r, w = Unix.socketpair ~cloexec:true PF_UNIX SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close r; Unix.close w) @@ fun () ->
  Unix.set_nonblock r;
  let alias = Unix.dup ~cloexec:true r in
  Fun.protect ~finally:(fun () -> Unix.close alias) @@ fun () ->
  Eio.Time.with_timeout_exn env#clock 5.0 @@ fun () ->
  Switch.run @@ fun sw ->
  let start fd =
    let armed, signal = Promise.create () in
    let done_ = Fiber.fork_promise ~sw (fun () ->
        Eio.Domain_manager.run env#domain_mgr @@ fun () ->
        let pipe, writer = Unix.pipe ~cloexec:true () in
        Fun.protect ~finally:(fun () -> Unix.close pipe; Unix.close writer) @@ fun () ->
        Switch.run @@ fun sw ->
        Fiber.fork_daemon ~sw (fun () -> Eio_unix.await_readable pipe; `Stop_daemon);
        let ready = Fiber.fork_promise ~sw (fun () -> Eio_unix.await_readable fd) in
        Eio_unix.await_writable fd;
        Promise.resolve signal ();
        Promise.await_exn ready) in
    Promise.await armed;
    done_
  in
  let first = start r in
  let second = start alias in
  ignore (Unix.single_write_substring w "x" 0 1);
  Promise.await_exn first;
  Promise.await_exn second

let invalid_readiness env () =
  let r, w = Unix.socketpair ~cloexec:true PF_UNIX SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close r; Unix.close w) @@ fun () ->
  let null = Unix.openfile Filename.null [O_RDONLY; O_CLOEXEC] 0 in
  Fun.protect ~finally:(fun () -> Unix.close null) @@ fun () ->
  Eio.Time.with_timeout_exn env#clock 5.0 @@ fun () ->
  Fiber.both
    (fun () ->
       Eio_unix.await_readable r;
       Alcotest.(check int) "socket became readable" 1 (Unix.read r (Bytes.create 1) 0 1))
    (fun () ->
       (match Eio_unix.await_readable null with
        | () -> Alcotest.fail "select accepted a non-console character device"
        | exception Unix.Unix_error (EBADF, _, _) -> ());
       ignore (Unix.write_substring w "x" 0 1))

let urgent_readiness env () =
  Switch.run @@ fun sw ->
  let with_fd flow fn = Eio_unix.Fd.use_exn "test" (Option.get (Eio_unix.Resource.fd_opt flow)) fn in
  let listener = Eio.Net.listen ~sw ~backlog:1 env#net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)) in
  with_fd listener (fun fd -> Unix.setsockopt_int fd Unix.SO_RCVBUF 4096);
  let client, peer = Fiber.pair
      (fun () -> Eio.Net.connect ~sw env#net (Eio.Net.listening_addr listener))
      (fun () -> fst (Eio.Net.accept ~sw listener)) in
  with_fd client @@ fun client ->
  with_fd peer @@ fun peer ->
  Unix.setsockopt_int client Unix.SO_SNDBUF 4096;
  let buf = Bytes.make 4096 'x' in
  let rec fill () =
    match Unix.send client buf 0 (Bytes.length buf) [] with
    | _ -> fill ()
    | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
      if Unix.select [] [client] [] 0.1 <> ([], [], []) then fill ()
  in
  fill ();
  ignore (Unix.send peer buf 0 1 [MSG_OOB]);
  let _, _, urgent = Unix.select [] [] [client] 2.0 in
  Alcotest.(check bool) "urgent data arrived" true (urgent <> []);
  let result = Eio.Time.with_timeout env#clock 0.1 (fun () -> Eio_unix.await_writable client; Ok ()) in
  Alcotest.(check bool) "urgent data leaves write waiting" true (result = Error `Timeout)

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

let busy_pipe_mode env () =
  Switch.run @@ fun sw ->
  let r, w = Eio.Process.pipe ~sw env#process_mgr in
  let raw_read = Eio_unix.Resource.fd_opt r |> Option.get |> Eio_unix.Fd.remove |> Option.get in
  let raw_write = Eio_unix.Resource.fd_opt w |> Option.get in
  let notify, wait = Unix.socketpair ~cloexec:true PF_UNIX SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close notify; Unix.close wait) @@ fun () ->
  let watchdog = Domain.spawn (fun () ->
      let ready, _, _ = Unix.select [wait] [] [] 2.0 in
      Unix.close raw_read;
      ready <> []) in
  let write fn = try fn () with Eio.Io _ | Unix.Unix_error _ -> () in
  Fiber.fork ~sw (fun () -> write (fun () -> Eio.Flow.write w [Cstruct.create (1024 * 1024)]));
  Eio_unix.await_readable raw_read;
  let timed_out write =
    match Eio.Time.with_timeout_exn env#clock 0.05 write with
    | () -> false
    | exception Eio.Time.Timeout -> true
    | exception (Eio.Io _ | Unix.Unix_error _) -> false
  in
  let managed, raw = Fiber.pair
      (fun () -> timed_out (fun () -> Eio.Flow.copy_string "x" w))
      (fun () -> timed_out (fun () -> ignore (Eio_windows.Low_level.write raw_write (Bytes.of_string "x") 0 1))) in
  ignore (Unix.single_write_substring notify "!" 0 1);
  let before_close = Domain.join watchdog in
  Alcotest.(check bool) "both writers cancelled before the watchdog" true (managed && raw && before_close)

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

let accept_finished_switch env () =
  Switch.run @@ fun sw ->
  let listener = Eio.Net.listen ~sw ~backlog:1 env#net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)) in
  let finished = Switch.run Fun.id in
  match Eio.Time.with_timeout_exn env#clock 2.0 (fun () -> Eio.Net.accept ~sw:finished listener) with
  | _ -> Alcotest.fail "accepted on a finished switch"
  | exception Invalid_argument _ -> ()

let finished_switch () =
  let sw = Switch.run Fun.id in
  let rejected f =
    match f () with
    | _ -> Alcotest.fail "accepted a finished switch"
    | exception Invalid_argument _ -> ()
  in
  rejected (fun () -> Eio_unix.pipe sw);
  let raw = Unix.socket ~cloexec:true PF_INET SOCK_DGRAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close raw) @@ fun () ->
  rejected (fun () -> Eio_unix.Net.import_socket_datagram ~sw ~close_unix:false raw);
  rejected (fun () -> Eio_unix.Net.import_socket_listening ~sw ~close_unix:false raw);
  rejected (fun () -> Eio_unix.Net.import_socket_stream ~sw ~close_unix:false raw);
  let before = handle_count () in
  for _ = 1 to 3 do
    rejected (fun () -> Eio_unix.Net.socketpair_stream ~sw ());
    rejected (fun () -> Eio_unix.Net.socketpair_datagram ~sw ())
  done;
  Alcotest.(check bool) "socketpair leaves no handles" true (handle_count () <= before)

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
    "accept checks the switch before waiting", accept_finished_switch;
    "pipe and socket setup report a finished switch to the caller", (fun _ -> finished_switch);
    "shutdown across domains and duplicated sockets", shared_socket;
    "overlapping accepts across domains", shared_listener;
    "TCP after first writer domain exits", tcp_after_domain_exit;
    "duplicated files share the cursor", (fun _ -> shared_file_cursor);
    "first completion in worker domain", first_completion_in_worker;
    "cancel pipe write", cancel_write;
    "TCP reads and writes after shutdown", local_shutdown;
    "positioned file errors", positioned_errors;
    "unconnected UDP ignores ICMP port unreachable", udp_reset;
    "unconnected UDP receive after shutdown", udp_shutdown;
    "switch between readiness and port waits", mixed_waits;
    "mixed readiness across domains on duplicated sockets", shared_readiness;
    "unsupported readiness leaves socket waiters alone", invalid_readiness;
    "TCP urgent data leaves write waiters alone", urgent_readiness;
    "synchronous pipe read cancellation", sync_read;
    "duplicated synchronous pipe readers", (fun _ -> sync_pipe_alias);
    "concurrent pipe writers can cancel independently", busy_pipe_mode;
    "child exit releases stdin and drains stdout", child_stdio;
    "single write reports its prefix", (fun _ -> write_prefix);
    "completed write racing cancellation", (fun _ -> completed_write);
    "pipe EOF", pipe_eof;
    "idle socket can register on another port", registration_release;
    "UDP completion and cancellation", udp;
    "pipe across domains", cross_domain_pipe;
    "duplicated pipe readers across domains", duplicated_pipe_readers;
    "cancel pipe read", cancel_read;
    "connection refusal", refused;
    "receive shutdown", (fun _ -> shutdown);
    "positioned IO preserves cursor and rights", positioned;
    "process output capture", process_capture;
    "shutdown through an idle TCP duplicate", idle_socket_alias;
    "remove during a cancelled pipe read", remove_during_read;
  ]
