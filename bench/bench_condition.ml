open Eio.Std

(* A publisher keeps updating a counter and signalling a condition.
   Two consumers read the counter whenever they get a signal.
   The producer stops after signalling [target], and the consumers stop after seeing it. *)

let n_iters = 100
let target = 100000

let run_publisher cond v =
  for i = 1 to target do
    Atomic.set v i;
    (*     traceln "set %d" i; *)
    Eio.Condition.broadcast cond
  done


let run_consumer cond v =
  try
    while true do
      Fiber.both
        (fun () -> Eio.Condition.await_no_mutex cond)
        (fun () ->
           let current = Atomic.get v in
           (* traceln "saw %d" current; *)
           if current = target then raise Exit
        )
    done
  with Exit -> ()

let run_bench ?domain_mgr ~clock () =
  let cond = Eio.Condition.create () in
  let v = Atomic.make 0 in
  let run_consumer () =
    match domain_mgr with
    | Some dm -> Eio.Domain_manager.run dm (fun () -> run_consumer cond v)
    | None -> run_consumer cond v
  in
  Gc.full_major ();
  let _minor0, prom0, _major0 = Gc.counters () in
  let t0 = Eio.Time.now clock in
  for _ = 1 to n_iters do
    Fiber.all [
      run_consumer;
      run_consumer;
      (fun () -> run_publisher cond v);
    ];
  done;
  let t1 = Eio.Time.now clock in
  let time_total = t1 -. t0 in
  let time_per_iter = time_total /. float n_iters in
  let _minor1, prom1, _major1 = Gc.counters () in
  let prom = prom1 -. prom0 in
  Printf.printf "%11b, %7.2f, %13.4f\n%!" (domain_mgr <> None) (1e3 *. time_per_iter) (prom /. float n_iters)

let main ~domain_mgr ~clock =
  Printf.printf "use_domains,  ms/iter, promoted/iter\n%!";
  run_bench ~clock ();
  run_bench ~domain_mgr ~clock ()

let () =
  Eio_main.run @@ fun env ->
  main
    ~domain_mgr:(Eio.Stdenv.domain_mgr env)
    ~clock:(Eio.Stdenv.clock env)
