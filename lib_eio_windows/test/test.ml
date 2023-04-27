module Timeout = struct
  let test clock () =
    let t0 = Unix.gettimeofday () in
    Eio.Time.sleep clock 0.01;
    let t1 = Unix.gettimeofday () in
    let diff = t1 -. t0 in
    if diff >= 0.01 then () else Alcotest.failf "Expected bigger difference than %f" diff


  let tests env = [
    "timeout", `Quick, test env#clock
  ]
end

module Net = struct
  open Eio.Std

  let read_all flow =
    let b = Buffer.create 100 in
    Eio.Flow.copy flow (Eio.Flow.buffer_sink b);
    Buffer.contents b

  let run_client ~sw ~net ~addr =
    traceln "Connecting to server...";
    let flow = Eio.Net.connect ~sw net addr in
    Eio.traceln "connected";
    Eio.Flow.copy_string "Hello from client" flow;
    Eio.Flow.shutdown flow `Send;
    let msg = read_all flow in
    msg

  let run_server ~sw msg socket =
    Eio.Net.accept_fork socket ~sw (fun flow _addr ->
      traceln "Server accepted connection from client";
      Fun.protect (fun () ->
        let msg = read_all flow in
        traceln "Server received: %S" msg
      ) ~finally:(fun () -> Eio.Flow.copy_string msg flow)
    )
    ~on_error:(function
      | ex -> traceln "Error handling connection: %s" (Printexc.to_string ex)
    )

  let test_client_server env () =
    Eio.Switch.run @@ fun sw ->
    let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8081) in
    let server = Eio.Net.listen env#net ~sw ~reuse_addr:true ~backlog:5 addr in
    let msg = "From the server" in
    Fiber.both
      (fun () -> run_server ~sw msg server)
      (fun () ->
        let client_msg = run_client ~sw ~net:env#net ~addr in 
        Alcotest.(check string) "same message" msg client_msg
      )


  let tests env = [
    "server-client", `Quick, test_client_server env
  ]
end

let () =
  Eio_windows.run @@ fun env ->
  Alcotest.run "eio_windows" [
    "net", Net.tests env;
    "timeout", Timeout.tests env
  ]