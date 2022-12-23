open Eio.Std

(* Prefix all trace output with "server: " *)
let traceln fmt = traceln ("server: " ^^ fmt)

module Read = Eio.Buf_read

(* Read one line from [client] and respond with "OK". *)
let handle_client flow addr =
  traceln "Accepted connection from %a" Eio.Net.Sockaddr.pp addr;
  (* We use a buffered reader because we may need to combine multiple reads
     to get a single line (or we may get multiple lines in a single read,
     although here we only use the first one). *)
  let from_client = Read.of_flow flow ~max_size:100 in
  traceln "Received: %S" (Read.line from_client);
  Eio.Flow.copy_string "OK" flow

(* Accept incoming client connections on [socket].
   We can handle multiple clients at the same time.
   Never returns (but can be cancelled). *)
let run socket =
  Switch.run @@ fun sw ->
  let rec serve () =
    Eio.Net.accept_fork ~sw socket handle_client
      ~on_error:(traceln "Error handling connection: %a" Fmt.exn);
    (* Loop to accept more connections (while the new fiber handles the
       one we just got): *)
    serve ()
  in
  serve ()
