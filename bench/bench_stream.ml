(* Some sender domains each run a bunch of fibers submitting items to a stream.
   Some receiver domains each run a single fiber accepting items from the stream.
   It also tests the single-domain case. *)

open Eio.Std

let n_sender_fibers = 10        (* Concurrent sending fibers per sending domain *)

(* Simulate other work in the domain, and also prevent it from going to sleep.
   Otherwise, we're just measuring how long it takes the OS to wake a sleeping thread. *)
let rec spin () =
  Fiber.yield ();
  spin ()

(* [n_fibers] fibers each send values [1..n_iters] to [stream]. *)
let run_sender ~n_fibers ~n_iters stream =
  Switch.run @@ fun sw ->
  Fiber.fork_daemon ~sw spin;
  for _ = 1 to n_fibers do
    Fiber.fork ~sw (fun () ->
        for i = 1 to n_iters do
          Eio.Stream.add stream i
        done
      )
  done

(* Read [n_iters] values from [stream] and add them to [total] (at the end). *)
let run_recv ~n_iters ~total stream =
  Switch.run @@ fun sw ->
  Fiber.fork_daemon ~sw spin;
  let rec aux acc = function
    | 0 -> acc
    | i -> aux (acc + Eio.Stream.take stream) (i - 1) in
  ignore (Atomic.fetch_and_add total (aux 0 n_iters) : int)

(* Run the tests using [n_sender_domains] additional domains to send (0 to send
   and receive in a single domain). When [n_sender_domains > 0], we also use
   that many receiver domains. *)
let run_bench ~domain_mgr ~clock ~n_send_domains ~n_iters ~capacity =
    let stream = Eio.Stream.create capacity in
    let total = Atomic.make 0 in                (* Total received (sanity check at the end) *)
    let n_senders = max 1 n_send_domains in
    let n_iters_total = (* Total number of items to be sent through [stream] *)
      n_iters * n_sender_fibers * n_senders
    in
    Gc.full_major ();
    let _minor0, prom0, _major0 = Gc.counters () in
    let t0 = Eio.Time.now clock in
    Switch.run (fun sw ->
        let run_sender () = run_sender ~n_fibers:n_sender_fibers ~n_iters stream in
        if n_send_domains > 0 then (
          for _ = 1 to n_send_domains do
            Fiber.fork ~sw (fun () -> Eio.Domain_manager.run domain_mgr run_sender)
          done
        ) else (
          Fiber.fork ~sw run_sender
        );
        let run_recv () = run_recv ~n_iters:(n_iters * n_sender_fibers) ~total stream in
        for _ = 1 to n_senders - 1 do
          Fiber.fork ~sw @@ fun () ->
          Eio.Domain_manager.run domain_mgr run_recv
        done;
        Fiber.fork ~sw run_recv
      );
    let t1 = Eio.Time.now clock in
    let total = Atomic.get total in
    let expected_total = n_senders * n_sender_fibers * (n_iters * (1 + n_iters) / 2) in
    assert (total = expected_total);
    let time_total = t1 -. t0 in
    let time_per_iter = time_total /. float n_iters_total in
    let _minor1, prom1, _major1 = Gc.counters () in
    let prom = prom1 -. prom0 in
    Printf.printf "%14d, %8d, %8d, %7.2f, %13.4f\n%!" n_send_domains n_iters_total capacity (1e9 *. time_per_iter) (prom /. float n_iters_total)

let main ~domain_mgr ~clock =
  Printf.printf "n_send_domains,  n_iters, capacity, ns/iter, promoted/iter\n%!";
  [0, 100_000;
   1, 100_000;
   2, 100_000;
   4, 100_000;
  ]
  |> List.iter (fun (n_send_domains, n_iters) ->
      [0; 1; 100] |> List.iter (fun capacity ->
          run_bench ~domain_mgr ~clock ~n_send_domains ~n_iters ~capacity
        )
    )

let () =
  Eio_main.run @@ fun env ->
  main
    ~domain_mgr:(Eio.Stdenv.domain_mgr env)
    ~clock:(Eio.Stdenv.clock env)
