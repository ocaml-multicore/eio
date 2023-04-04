(* A multi-domain server handles HTTP-like requests from many clients running across multiple domains. *)

open Eio.Std

(* Note: this is not a real HTTP parser! *)
let key_char = function
  | 'A'..'Z' | 'a'..'z' | '-' -> true
  | _ -> false

let parse_headers r =
  let len = ref (-1) in
  let rec aux () =
    let key = Eio.Buf_read.take_while key_char r in
    if key = "" then Eio.Buf_read.string "\r\n" r
    else (
      Eio.Buf_read.char ':' r;
      let value = Eio.Buf_read.line r in
      if key = "Content-Length" then len := int_of_string (String.trim value);
      aux ()
    )
  in
  aux ();
  !len

let handle_connection conn _addr =
  Eio.Buf_write.with_flow conn @@ fun w ->
  let rec requests r =
    let _req = Eio.Buf_read.line r in
    let len = parse_headers r in
    let body = Eio.Buf_read.take len r in
    let response = body ^ " / received" in
    Eio.Buf_write.string w "HTTP/1.1 200 OK\r\n";
    Eio.Buf_write.string w (Printf.sprintf "Content-Length: %d\r\n" (String.length response));
    Eio.Buf_write.string w "\r\n";
    Eio.Buf_write.string w response;
    if not (Eio.Buf_read.at_end_of_input r) then requests r
  in
  Eio.Buf_read.parse_exn requests conn ~max_size:max_int

let run_client ~n_requests id conn =
  let total = ref 0 in
  let r = Eio.Buf_read.of_flow conn ~max_size:max_int in
  Eio.Buf_write.with_flow conn @@ fun w ->
  for i = 1 to n_requests do
    let msg = Printf.sprintf "%s / request %d" id i in
    Eio.Buf_write.string w "POST / HTTP/1.1\r\n";
    Eio.Buf_write.string w "Host: localhost:8085\r\n";
    Eio.Buf_write.string w "User-Agent: bench_server\r\n";
    Eio.Buf_write.string w "Connection: keep-alive\r\n";
    Eio.Buf_write.string w (Printf.sprintf "Content-Length: %d\r\n" (String.length msg));
    Eio.Buf_write.string w "\r\n";
    Eio.Buf_write.string w msg;
    let status = Eio.Buf_read.line r in
    assert (status = "HTTP/1.1 200 OK");
    let len = parse_headers r in
    let body = Eio.Buf_read.take len r in
    assert (body = msg ^ " / received");
    incr total
  done;
  !total

let main net domain_mgr ~n_client_domains ~n_server_domains ~n_connections_per_domain ~n_requests_per_connection =
  let total = Atomic.make 0 in
  let t0 = Unix.gettimeofday () in
  Switch.run (fun sw ->
      let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8085) in
      let backlog = n_connections_per_domain * n_client_domains in
      let server_socket = Eio.Net.listen ~reuse_addr:true ~backlog ~sw net addr in
      Fiber.fork_daemon ~sw (fun () ->
          Eio.Net.run_server server_socket handle_connection
            ~additional_domains:(domain_mgr, n_server_domains - 1)
            ~on_error:raise
        );
      for domain = 1 to n_client_domains do
        Fiber.fork ~sw (fun () ->
            Eio.Domain_manager.run domain_mgr (fun () ->
                Switch.run @@ fun sw ->
                for i = 1 to n_connections_per_domain do
                  Fiber.fork ~sw (fun () ->
                      let id = Printf.sprintf "domain %d / conn %d" domain i in
                      let conn = Eio.Net.connect ~sw net addr in
                      let requests = run_client ~n_requests:n_requests_per_connection id conn in
                      ignore (Atomic.fetch_and_add total requests : int)
                    )
                done
              )
          )
      done
    );
  let t1 = Unix.gettimeofday () in
  Fmt.pr "clients, servers, requests, requests/s@.";
  let requests = n_connections_per_domain * n_client_domains * n_requests_per_connection in
  assert (requests = Atomic.get total);
  let req_per_s = float requests /. (t1 -. t0) in
  Fmt.pr "%7d, %7d, %8d, %.1f@." n_client_domains n_server_domains requests req_per_s

let () =
  print_endline "";     (* work around dune bug *)
  Eio_main.run @@ fun env ->
  let main = main env#net env#domain_mgr in
  match Sys.argv with
  | [| _ |] -> main ~n_client_domains:4 ~n_server_domains:4 ~n_connections_per_domain:25 ~n_requests_per_connection:1000
  | [| _; n_client_domains; n_server_domains; n_connections_per_domain; n_requests_per_connection |] ->
    main
      ~n_client_domains:(int_of_string n_client_domains)
      ~n_server_domains:(int_of_string n_server_domains)
      ~n_connections_per_domain:(int_of_string n_connections_per_domain)
      ~n_requests_per_connection:(int_of_string n_requests_per_connection)
  | _ -> Fmt.failwith "usage: bench_http [clients servers connections_per_domain requests_per_connection]"
