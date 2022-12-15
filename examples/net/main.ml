open Eio.Std

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080)

(* Run a server and a test client, communicating using [net]. *)
let main ~net =
  Switch.run @@ fun sw ->
  (* We create the listening socket first so that we can be sure it is ready
     as soon as the client wants to use it. *)
  let listening_socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  (* Start the server running in a new fiber.
     Using [fork_daemon] here means that it will be stopped once the client is done
     (we don't wait for it to finish because it will keep accepting new connections forever). *)
  Fiber.fork_daemon ~sw (fun () -> Server.run listening_socket);
  (* Test the server: *)
  Client.run ~net ~addr

let () =
  Eio_main.run @@ fun env ->
  main ~net:(Eio.Stdenv.net env)
