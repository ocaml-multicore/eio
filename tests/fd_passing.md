# Setting up the environment

```ocaml
# #require "eio_main";;
```

```ocaml
open Eio.Std

let ( / ) = Eio.Path.( / )

let run ?clear:(paths = []) fn =
  Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  List.iter (fun p -> Eio.Path.rmtree ~missing_ok:true (cwd / p)) paths;
  fn env

let read flow =
  let buf = Cstruct.of_string "?" in
  Eio.Flow.read_exact flow buf;
  traceln "Got %S" (Cstruct.to_string buf)
```

```ocaml
(* Send [to_send] to [w] and get it from [r], then read it. *)
let test ~to_send r w =
  Switch.run @@ fun sw ->
  Fiber.both
    (fun () -> Eio_unix.Net.send_msg w [Cstruct.of_string "x"] ~fds:to_send)
    (fun () ->
       let buf = Cstruct.of_string "?" in
       let got, fds = Eio_unix.Net.recv_msg_with_fds ~sw r ~max_fds:2 [buf] in
       let msg = Cstruct.to_string buf ~len:got in
       traceln "Got: %S plus %d FDs" msg (List.length fds);
       fds |> List.iter (fun fd ->
         Eio_unix.Fd.use_exn "read" fd @@ fun fd ->
         let len = Unix.lseek fd 0 Unix.SEEK_CUR in
         ignore (Unix.lseek fd 0 Unix.SEEK_SET : int);
         traceln "Read: %S" (really_input_string (Unix.in_channel_of_descr fd) len);
       )
    )

let with_tmp_file dir id fn =
  let path = (dir / (Printf.sprintf "tmp-%s.txt" id)) in
  Eio.Path.with_open_out path ~create:(`Exclusive 0o600) @@ fun file ->
  Fun.protect
    (fun () ->
       Eio.Flow.copy_string id file;
       fn (Option.get (Eio_unix.Resource.fd_opt file))
    )
    ~finally:(fun () -> Eio.Path.unlink path)

let macos =
  run @@ fun env ->
  match Eio.Process.parse_out env#process_mgr Eio.Buf_read.line ["uname"] with
  | "Darwin" -> true
  | _ -> false
```

## Tests

Using a socket-pair:

```ocaml
# run ~clear:["tmp-foo.txt"; "tmp-bar.txt"] @@ fun env ->
  with_tmp_file env#cwd "foo" @@ fun fd1 ->
  with_tmp_file env#cwd "bar" @@ fun fd2 ->
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.Net.socketpair_stream ~sw ~domain:PF_UNIX ~protocol:0 () in
  test ~to_send:[fd1; fd2] r w;;
+Got: "x" plus 2 FDs
+Read: "foo"
+Read: "bar"
- : unit = ()
```

Using named sockets:

```ocaml
# run ~clear:["tmp-foo.txt"] @@ fun env ->
  let net = env#net in
  with_tmp_file env#cwd "foo" @@ fun fd ->
  Switch.run @@ fun sw ->
  let addr = `Unix "test.socket" in
  let server = Eio.Net.listen ~sw net ~reuse_addr:true ~backlog:1 addr in
  let r, w = Fiber.pair
    (fun () -> Eio.Net.connect ~sw net addr)
    (fun () -> fst (Eio.Net.accept ~sw server))
  in
  test ~to_send:[fd] r w;;
+Got: "x" plus 1 FDs
+Read: "foo"
- : unit = ()
```

When sharing an open file with another process, there is the risk that it may change the blocking mode.
Check that socket operations are always non-blocking (because they use `MSG_DONTWAIT`), even if the mode gets changed:

```ocaml
# if macos then mdx_skip "MSG_DONTWAIT isn't supported on macos for sendmsg";;
- : unit = ()

# run @@ fun env ->
  let a_unix, b_unix = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
  Switch.run @@ fun sw ->
  let a = Eio_unix.Net.import_socket_stream ~sw ~close_unix:true a_unix in
  let b = Eio_unix.Net.import_socket_stream ~sw ~close_unix:true b_unix in
  (* Warm-up: let the backend set the sockets to non-blocking *)
  Fiber.both
    (fun () -> read b)
    (fun () -> Eio.Flow.copy_string "1" a);
  (* Simulate another process changing the mode behind our back *)
  Unix.clear_nonblock b_unix;
  Fiber.both
    (fun () -> read b)
    (fun () -> Eio.Flow.copy_string "2" a);
  Fiber.first
    (fun () -> while true do Eio.Flow.write b [Cstruct.create 1_000_000] done)
    (fun () -> Fiber.yield (); traceln "Write cancelled")
+Got "1"
+Got "2"
+Write cancelled
- : unit = ()
```
