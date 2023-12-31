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
