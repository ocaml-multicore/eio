open Eio.Std

let n_iters = 10_000_000

let main ~clock =
  Printf.printf "n_iters, ns/iter, promoted/iter\n%!";
  let stream = Eio.Stream.create 10 in
  Gc.full_major ();
  let _minor0, prom0, _major0 = Gc.counters () in
  let t0 = Eio.Time.now clock in
  Fibre.both
    (fun () ->
       for i = 1 to n_iters do
         Eio.Stream.add stream i
       done
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
  Printf.printf "%5d, %.2f, %7.4f\n%!" n_iters (1e9 *. time_per_iter) (prom /. float n_iters)

let () =
  Eio_main.run @@ fun env ->
  main ~clock:(Eio.Stdenv.clock env)
