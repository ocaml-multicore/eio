open Eio.Std

let n_domains = 4
let n_rounds = 100
let n_procs_per_round_per_domain = 100 / n_domains

let run_in_domain mgr =
  let echo n = Eio.Process.parse_out mgr Eio.Buf_read.line ["sh"; "-c"; "echo " ^ string_of_int n] in
  Switch.run @@ fun sw ->
  for j = 1 to n_procs_per_round_per_domain do
    Fiber.fork ~sw (fun () ->
        let result = echo j in
        assert (int_of_string result = j);
        (* traceln "OK: %d" j *)
      )
  done

let main ~dm mgr =
  let t0 = Unix.gettimeofday () in
  for i = 1 to n_rounds do
    Switch.run ~name:"round" (fun sw ->
        for _ = 1 to n_domains - 1 do
          Fiber.fork ~sw (fun () -> Eio.Domain_manager.run dm (fun () -> run_in_domain mgr))
        done;
        Fiber.fork ~sw (fun () -> run_in_domain mgr);
      );
    if true then traceln "Finished round %d/%d" i n_rounds
  done;
  let t1 = Unix.gettimeofday () in
  let n_procs = n_rounds * n_procs_per_round_per_domain * n_domains in
  traceln "Finished process stress test: ran %d processes in %.2fs (using %d domains)" n_procs (t1 -. t0) n_domains

let () =
  Eio_main.run @@ fun env ->
  main ~dm:env#domain_mgr  env#process_mgr
