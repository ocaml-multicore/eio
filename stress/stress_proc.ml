open Eio.Std

let n_rounds = 100
let n_procs_per_round = 100

let main mgr =
  let echo n = Eio.Process.parse_out mgr Eio.Buf_read.line ["sh"; "-c"; "echo " ^ string_of_int n] in
  let t0 = Unix.gettimeofday () in
  for i = 1 to n_rounds do
    Switch.run @@ fun sw ->
    for j = 1 to n_procs_per_round do
      Fiber.fork ~sw (fun () ->
          let result = echo j in
          assert (int_of_string result = j);
          (* traceln "OK: %d" j *)
        )
    done;
    if false then traceln "Finished round %d/%d" i n_rounds
  done;
  let t1 = Unix.gettimeofday () in
  let n_procs = n_rounds * n_procs_per_round in
  traceln "Finished process stress test: ran %d processes in %.2fs" n_procs (t1 -. t0)

let () =
  Eio_main.run @@ fun env ->
  main env#process_mgr
