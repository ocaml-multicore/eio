open Eio.Std

(* Prefix all trace output with "client: " *)
let traceln fmt = traceln ("client: " ^^ fmt)

module Read = Eio.Buf_read
module Write = Eio.Buf_write

(* Connect to [addr] on [net], send a message and then read the reply. *)
let run ~net ~addr =
  traceln "Connecting to server at %a..." Eio.Net.Sockaddr.pp addr;
  Switch.run @@ fun sw ->
  let flow = Eio.Net.connect ~sw net addr in
  (* We use a buffered writer here so we can create the message in multiple
     steps but still send it efficiently as a single packet: *)
  Write.with_flow flow @@ fun to_server ->
  Write.string to_server "Hello";
  Write.char to_server ' ';
  Write.string to_server "from client\n";
  let reply = Read.(parse_exn take_all) flow ~max_size:100 in
  traceln "Got reply %S" reply
