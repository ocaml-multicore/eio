(* A client opens a connection to an echo service and sends a load of data via it. *)

open Eio.Std

let chunk_size = 1 lsl 16
let n_chunks = 10000
let n_bytes = n_chunks * chunk_size

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

let run _env =
  [
    time "default" (fun sock -> Eio.Flow.copy sock sock);
    time "buf_read" (fun sock ->
        let r = Eio.Buf_read.of_flow sock ~initial_size:(64 * 1024) ~max_size:(64 * 1024) |> Eio.Buf_read.as_flow in
        Eio.Flow.copy r sock);
  ]
