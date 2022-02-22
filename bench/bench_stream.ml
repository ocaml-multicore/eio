open Eio.Std

let run_sender ~n_iters stream =
  for i = 1 to n_iters do
    Eio.Stream.add stream i
  done

let run_bench ~domain_mgr ~clock ~use_domains ~n_iters ~capacity =
    let stream = Eio.Stream.create capacity in
    Gc.full_major ();
    let _minor0, prom0, _major0 = Gc.counters () in
    let t0 = Eio.Time.now clock in
    Fiber.both
      (fun () ->
         if use_domains then (
           Eio.Domain_manager.run domain_mgr @@ fun () ->
           run_sender ~n_iters stream
         ) else (
           run_sender ~n_iters stream
         )
      )
      (fun () ->
         for i = 1 to n_iters do
           let j = Eio.Stream.take stream in
           assert (i = j)
         done
      );
    let t1 = Eio.Time.now clock in
    let time_total = t1 -. t0 in
    let time_per_iter = time_total /. float n_iters in
    let _minor1, prom1, _major1 = Gc.counters () in
    let prom = prom1 -. prom0 in
    Printf.printf "%11b, %8d, %8d, %7.2f, %13.4f\n%!" use_domains n_iters capacity (1e9 *. time_per_iter) (prom /. float n_iters)

let main ~domain_mgr ~clock =
  Printf.printf "use_domains,  n_iters, capacity, ns/iter, promoted/iter\n%!";
  [false, 10_000_000;
   true,   1_000_000]
  |> List.iter (fun (use_domains, n_iters) ->
      [0; 1; 10; 100; 1000] |> List.iter (fun capacity ->
          run_bench ~domain_mgr ~clock ~use_domains ~n_iters ~capacity
        )
    )

let () =
  Eio_main.run @@ fun env ->
  main
    ~domain_mgr:(Eio.Stdenv.domain_mgr env)
    ~clock:(Eio.Stdenv.clock env)
