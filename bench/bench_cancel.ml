open Eio.Std

(* The main domain spawns two other domains, connected to each by a stream.
   It keeps reading from whichever stream is ready first, cancelling the other read.
   This tests the time needed to set up and tear down cancellation contexts and
   tests that cancellation can happen in parallel with success. *)

let n_iters = 100_000

let run_sender stream =
  for i = 1 to n_iters do
    Eio.Stream.add stream i
  done

let run_bench ?domain_mgr ~clock () =
  let stream1 = Eio.Stream.create 1 in
  let stream2 = Eio.Stream.create 1 in
  let run_sender stream () =
    match domain_mgr with
    | Some dm -> Eio.Domain_manager.run dm (fun () -> run_sender stream)
    | None -> run_sender stream
  in
  let name str =
    match domain_mgr with
    | Some _ -> str ^ "/separate domains"
    | None -> str ^ "/single domain"
  in
  Gc.full_major ();
  let t0 = Eio.Time.now clock in
  try
    Switch.run (fun sw ->
        Fiber.fork ~sw (run_sender stream1);
        Fiber.fork ~sw (run_sender stream2);
        for _ = 1 to n_iters do
          ignore @@
          Fiber.first
            (fun () -> Eio.Stream.take stream1)
            (fun () -> Eio.Stream.take stream2)
        done;
        raise Exit
      )
  with Exit ->
    let t1 = Eio.Time.now clock in
    let time_total = t1 -. t0 in
    let time_per_iter = time_total /. float n_iters in
    Metric.create
      (name "take-first")
      (`Float (1e9 *. time_per_iter)) "ns"
      "Time to take from one of two streams"

let main ~domain_mgr ~clock =
  let m1 = run_bench ~clock () in
  let m2 = run_bench ~domain_mgr ~clock () in
  [m1; m2]

let run env =
  main
    ~domain_mgr:(Eio.Stdenv.domain_mgr env)
    ~clock:(Eio.Stdenv.clock env)
