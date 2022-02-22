open Eio.Std

let run_sender ~n_iters ~batch_size ~ack sem =
  for i = 1 to n_iters do
    Eio.Semaphore.release sem;
    if i mod batch_size = 0 then
      Eio.Semaphore.acquire ack
  done

let run_bench ~domain_mgr ~clock ~use_domains ~n_iters ~batch_size =
  let sem = Eio.Semaphore.make 0 in
  let ack = Eio.Semaphore.make 0 in
  Gc.full_major ();
  let _minor0, prom0, _major0 = Gc.counters () in
  let t0 = Eio.Time.now clock in
  Fiber.both
    (fun () ->
       if use_domains then (
         Eio.Domain_manager.run domain_mgr @@ fun () ->
         run_sender ~n_iters ~batch_size ~ack sem
       ) else (
         run_sender ~n_iters ~batch_size ~ack sem
       )
    )
    (fun () ->
       for i = 1 to n_iters do
         Eio.Semaphore.acquire sem;
         if i mod batch_size = 0 then
           Eio.Semaphore.release ack
       done
    );
  let t1 = Eio.Time.now clock in
  let time_total = t1 -. t0 in
  let time_per_iter = time_total /. float n_iters in
  let _minor1, prom1, _major1 = Gc.counters () in
  let prom = prom1 -. prom0 in
  Printf.printf "%11b, %8d, %3d, %8.2f, %13.4f\n%!" use_domains n_iters batch_size (1e9 *. time_per_iter) (prom /. float n_iters)

let main ~domain_mgr ~clock =
  Printf.printf "use_domains,   n_iters,  batch, ns/iter, promoted/iter\n%!";
  [false, 1_000_000, 1;
   false, 1_000_000, 10;
   false, 1_000_000, 100;
   true,    100_000, 1;
   true,    100_000, 10;
   true,    100_000, 100]
  |> List.iter (fun (use_domains, n_iters, batch_size) ->
      run_bench ~domain_mgr ~clock ~use_domains ~n_iters ~batch_size
    )

let () =
  Eio_main.run @@ fun env ->
  main
    ~domain_mgr:(Eio.Stdenv.domain_mgr env)
    ~clock:(Eio.Stdenv.clock env)
