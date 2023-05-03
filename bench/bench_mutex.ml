open Eio.Std

let v = ref 0

let run_worker ~iters_per_thread mutex =
  for _ = 1 to iters_per_thread do
    Eio.Mutex.lock mutex;
    let x = !v in
    v := x + 1;
    Fiber.yield ();
    assert (!v = x + 1);
    v := x;
    Eio.Mutex.unlock mutex;
  done

let run_bench ~domain_mgr ~clock ~use_domains ~iters_per_thread ~threads =
  let mutex = Eio.Mutex.create () in
  Gc.full_major ();
  let t0 = Eio.Time.now clock in
  Switch.run (fun sw ->
      for _ = 1 to threads do
        Fiber.fork ~sw (fun () ->
            if use_domains then (
              Eio.Domain_manager.run domain_mgr @@ fun () ->
              run_worker ~iters_per_thread mutex
            ) else (
              run_worker ~iters_per_thread mutex
            )
          )
      done
    );
  assert (!v = 0);
  let t1 = Eio.Time.now clock in
  let time_total = t1 -. t0 in
  let n_iters = iters_per_thread * threads in
  let time_per_iter = time_total /. float n_iters in
  Metric.create
    (Printf.sprintf "iterations=%d threads=%d" n_iters threads)
    (`Float (1e9 *. time_per_iter)) "ns" "Time to update a shared counter"

let main ~domain_mgr ~clock =
  [false, 1_000_000, 1;
   false, 1_000_000, 2;
   false,   100_000, 8;
   true,    100_000, 1;
   true,     10_000, 2;
   true,     10_000, 8]
  |> List.map (fun (use_domains, iters_per_thread, threads) ->
      run_bench ~domain_mgr ~clock ~use_domains ~iters_per_thread ~threads)

let run env =
  main
    ~domain_mgr:(Eio.Stdenv.domain_mgr env)
    ~clock:(Eio.Stdenv.clock env)
