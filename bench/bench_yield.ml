open Eio.Std

let n_fibres = [1; 2; 3; 4; 5; 10; 20; 30; 40; 50; 100; 500; 1000; 10000]

let main ~clock =
  Printf.printf "n_fibers, ns/iter, promoted/iter\n%!";
  n_fibres |> List.iter (fun n_fibres ->
      let n_iters = 1000000 / n_fibres in
      Gc.full_major ();
      let _minor0, prom0, _major0 = Gc.counters () in
      let t0 = Eio.Time.now clock in
      Switch.run (fun sw ->
          for _ = 1 to n_fibres do
            Fibre.fork ~sw (fun () ->
                for _ = 1 to n_iters do
                  Fibre.yield ()
                done
              )
          done
        );
      let t1 = Eio.Time.now clock in
      let time_total = t1 -. t0 in
      let n_total = n_fibres * n_iters in
      let time_per_iter = time_total /. float n_total in
      let _minor1, prom1, _major1 = Gc.counters () in
      let prom = prom1 -. prom0 in
      Printf.printf "%8d, % 7.2f, % 13.4f\n%!" n_fibres (1e9 *. time_per_iter) (prom /. float n_total)
    )

let () =
  Eio_main.run @@ fun env ->
  main ~clock:(Eio.Stdenv.clock env)
