(* A client opens a connection to an echo service and sends a load of data via it. *)

open Eio.Std

let chunk_size = 1 lsl 16
let n_chunks = 10000
let n_bytes = n_chunks * chunk_size

let rec await pid =
  match Unix.waitpid [] pid with
  | (_pid, status) -> status
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> await pid

let run prog args =
  await (Unix.create_process prog (Array.of_list (prog :: args)) Unix.stdin Unix.stdout Unix.stderr)

let check_status = function
  | Unix.WEXITED 0 -> ()
  | _ -> assert false

let generate_file name =
  run "fallocate" [ "-l"; string_of_int n_bytes; name ]
  |> check_status

let run_client sock =
  Fiber.both
    (fun () ->
       let chunk = Cstruct.create chunk_size in
       for _ = 1 to n_chunks do
         Eio.Flow.write sock [chunk]
       done;
       Eio.Flow.shutdown sock `Send
    )
    (fun () ->
       let chunk = Cstruct.create chunk_size in
       for _ = 1 to n_chunks do
         Eio.Flow.read_exact sock chunk
       done
    )

let time name service =
  Switch.run ~name @@ fun sw ->
  let client_sock, server_sock = Eio_unix.Net.socketpair_stream ~sw () in
  let t0 = Unix.gettimeofday () in
  Fiber.both
    (fun () -> service server_sock)
    (fun () -> run_client client_sock);
  let t1 = Unix.gettimeofday () in
  let time = t1 -. t0 in
  let bytes_per_second = float n_bytes /. time in
  traceln "%s: %.2f MB/s" name (bytes_per_second /. 1024. /. 1024.);
  Metric.create name (`Float bytes_per_second) "bytes/s" (name ^ " Flow.copy")

let time_fs fs name service =
  let ( / ) = Eio.Path.(/) in
  let fname_in = "cptest.in" in
  let fname_out ="cptest.out" in
  if not (Sys.file_exists fname_in) then generate_file fname_in;
  Eio.Path.with_open_in (fs / fname_in) @@ fun inflow ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / fname_out) @@ fun outflow ->
  let t0 = Unix.gettimeofday () in
  service inflow outflow;
  let t1 = Unix.gettimeofday () in
  let time = t1 -. t0 in
  let bytes_per_second = float n_bytes /. time in
  traceln "%s: %.2f MB/s" name (bytes_per_second /. 1024. /. 1024.);
  at_exit (fun () -> try Sys.remove fname_in with _ -> ());
  Sys.remove fname_out;
  Metric.create name (`Float bytes_per_second) "bytes/s" (name ^ " Flow.copy")

let run env =
  [
    time_fs env#fs "default_fs" (fun inflow outflow -> Eio.Flow.copy inflow outflow);
    time "default" (fun sock -> Eio.Flow.copy sock sock);
    time "buf_read" (fun sock ->
        let r = Eio.Buf_read.of_flow sock ~initial_size:(64 * 1024) ~max_size:(64 * 1024) |> Eio.Buf_read.as_flow in
        Eio.Flow.copy r sock);
  ]
