open Eio.Std

let n_fibers = [1; 2; 3; 4; 5; 10; 20; 30; 40; 50; 100; 500; 1000; 10000]

let main ~clock =
  Printf.printf "n_fibers, ns/iter, promoted/iter\n%!";
  n_fibers |> List.iter (fun n_fibers ->
      let n_iters = 1000000 / n_fibers in
      Gc.full_major ();
      let _minor0, prom0, _major0 = Gc.counters () in
      let t0 = Eio.Time.now clock in
      Switch.run (fun sw ->
          for _ = 1 to n_fibers do
            Fiber.fork ~sw (fun () ->
                for _ = 1 to n_iters do
                  Eio_linux.Low_level.noop ()
                done
              )
          done
        );
      let t1 = Eio.Time.now clock in
      let time_total = t1 -. t0 in
      let n_total = n_fibers * n_iters in
      let time_per_iter = time_total /. float n_total in
      let _minor1, prom1, _major1 = Gc.counters () in
      let prom = prom1 -. prom0 in
      Printf.printf "%5d, %.2f, %7.4f\n%!" n_fibers (1e9 *. time_per_iter) (prom /. float n_total)
    )

let () =
  Eio_linux.run @@ fun env ->
  main ~clock:(Eio.Stdenv.clock env)
