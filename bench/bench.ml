open Eio.Std

let n_fibres = [1; 2; 3; 4; 5; 10; 20; 30; 40; 50; 100; 500; 1000; 10000]

let main ~clock =
  traceln "n_fibers, ns/iter";
  n_fibres |> List.iter (fun n_fibres ->
      let n_iters = 1000000 / n_fibres in
      let t0 = Eio.Time.now clock in
      Switch.run (fun sw ->
          for _ = 1 to n_fibres do
            Fibre.fork_ignore ~sw (fun () ->
                for _ = 1 to n_iters do
                  Eio_linux.noop ()
                done
              )
          done
        );
      let t1 = Eio.Time.now clock in
      let time_total = t1 -. t0 in
      let n_total = n_fibres * n_iters in
      let time_per_iter = time_total /. float n_total in
      traceln "%d, %.2f" n_fibres (1e9 *. time_per_iter)
      (* traceln "%d fibres did %d noops in %.2f seconds : %.2f ns/iter"
           n_fibres n_iters time_total (1e9 *. time_per_iter) *)
    )

let () =
  Eio_main.run @@ fun env ->
  main ~clock:(Eio.Stdenv.clock env)
