type job = Pack : {
    fn : unit -> 'a;
    w : ('a, exn) Result.t Promise.u;
    weight : int;
  } -> job

type t = {
  queue : job Sync.t;
}

let max_capacity = 1_000_000

let max_capacity_f = float max_capacity

(* This function is the core of executor_pool.ml.
   Each worker runs in its own domain,
   taking jobs from [queue] whenever it has spare capacity. *)
let run_worker { queue } =
  Switch.run ~name:"run_worker" @@ fun sw ->
  let capacity = ref 0 in
  let condition = Condition.create () in
  (* The main worker loop. *)
  let rec loop () =
    while !capacity >= max_capacity do Condition.await_no_mutex condition done;
    match Sync.take queue with
    | Error `Closed -> `Stop_daemon
    | Ok (Pack { fn; w; weight }) ->
      capacity := !capacity + weight;
      Option.iter (Promise.resolve_error w) (Switch.get_error sw);
      Fiber.fork ~sw (fun () ->
          Promise.resolve w (try Ok (fn ()) with ex -> Error ex);
          capacity := !capacity - weight;
          Condition.broadcast condition
        );
      (* Give a chance to other domains to start waiting on [queue]
         before the current thread blocks on [Sync.take] again. *)
      Fiber.yield ();
      (loop [@tailcall]) ()
  in
  loop ()

let create ~sw ~domain_count domain_mgr =
  let queue = Sync.create () in
  let t = { queue } in
  Switch.on_release sw (fun () -> Sync.close queue);
  for _ = 1 to domain_count do
    (* Workers run as daemons to not hold the user's switch from completing.
       It's up to the user to hold the switch open (and thus, the executor pool)
       by blocking on the jobs issued to the pool. *)
    Fiber.fork_daemon ~sw (fun () ->
        Domain_manager.run domain_mgr (fun () ->
            run_worker t))
  done;
  t

let enqueue { queue } ~weight fn =
  if not (weight >= 0. && weight <= 1.) (* Handles NaN *)
  then Fmt.invalid_arg "Executor_pool: weight %g not >= 0.0 && <= 1.0" weight
  else (
    let weight = Float.to_int (weight *. max_capacity_f) in
    let p, w = Promise.create () in
    Sync.put queue (Pack { fn; w; weight });
    p
  )

let submit t ~weight fn =
  enqueue t ~weight fn |> Promise.await

let submit_exn t ~weight fn =
  enqueue t ~weight fn |> Promise.await_exn

let submit_fork ~sw t ~weight fn =
  (* [enqueue] blocks until the job is accepted, so we have to fork here. *)
  Fiber.fork_promise ~sw (fun () -> submit_exn t ~weight fn)
