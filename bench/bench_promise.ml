open Eio.Std

type request = {
  req_body : int;
  response : response option Promise.u;
}
and response = {
  resp_body : int;
  next_request : request Promise.u;
}

(* Simulate other work in the domain, and also prevent it from going to sleep.
   Otherwise, we're just measuring how long it takes the OS to wake a sleeping thread. *)
let rec spin () =
  Fiber.yield ();
  spin ()

(* A client and server exchange these payload values.
   Each contains the current message and a resolver which the other party can use to reply. *)

let rec run_server ~n_iters ~i r =
  (* Set up reply channel *)
  let p2, r2 = Promise.create () in
  (* Send i and next_request channel to client *)
  Promise.resolve r (Some { resp_body = i; next_request = r2 });
  (* Await client's response, with new send channel *)
  let { req_body; response } = Promise.await p2 in
  assert (req_body = i);
  if i < n_iters then
    run_server ~n_iters ~i:(succ i) response
  else
    Promise.resolve response None

let rec run_client ~n_iters ~i p =
  (* Wait for message and reply channel from server *)
  match Promise.await p with
  | Some { resp_body; next_request } ->
    assert (resp_body = i);
    (* Create new channel for next message *)
    let p2, r2 = Promise.create () in
    (* Send reply message and new channel to the server *)
    Promise.resolve next_request { req_body = i; response = r2 };
    run_client ~n_iters ~i:(succ i) p2
  | None ->
    assert (i = n_iters + 1)

let bench_resolved ~clock ~n_iters =
  let t0 = Eio.Time.now clock in
  let p = Promise.create_resolved 1 in
  let t = ref 0 in
  for _ = 1 to n_iters do
    t := !t + Promise.await p;
  done;
  let t1 = Eio.Time.now clock in
  assert (!t = n_iters);
  Metric.create
    "read-resolved"
    (`Float (1e9 *. (t1 -. t0) /. float n_iters)) "ns"
    "Time to read a resolved promise"

let maybe_spin v fn =
  if v then Fiber.first spin fn
  else fn ()

let run_bench ~domain_mgr ~spin ~clock ~use_domains ~n_iters =
  let init_p, init_r = Promise.create () in
  Gc.full_major ();
  let t0 = Eio.Time.now clock in
  Fiber.both
    (fun () ->
       if use_domains then (
         Eio.Domain_manager.run domain_mgr @@ fun () ->
         maybe_spin spin (fun () -> run_server ~n_iters ~i:0 init_r)
       ) else (
         maybe_spin spin (fun () -> run_server ~n_iters ~i:0 init_r)
       )
    )
    (fun () ->
       maybe_spin spin (fun () -> run_client ~n_iters ~i:0 init_p)
    );
  let t1 = Eio.Time.now clock in
  let time_total = t1 -. t0 in
  let time_per_iter = time_total /. float n_iters in
  let domains_label =
    if use_domains then
      if spin then "with-spin"
      else "without-spin"
    else "no"
  in
  Metric.create
    (Printf.sprintf "iterations:%d domains:%s" n_iters domains_label)
    (`Float (1e9 *. time_per_iter)) "ns"
    "Time to round-trip a request/reply"

let main ~domain_mgr ~clock =
  let resolved = bench_resolved ~clock ~n_iters:(10_000_000) in
  let metrics = [false, false, 1_000_000;
   true,  true,    100_000;
   true,  false,   100_000]
  |> List.map (fun (use_domains, spin, n_iters) ->
      run_bench ~domain_mgr ~spin ~clock ~use_domains ~n_iters
  ) in
  resolved :: metrics

let run env =
  main
    ~domain_mgr:(Eio.Stdenv.domain_mgr env)
    ~clock:(Eio.Stdenv.clock env)
