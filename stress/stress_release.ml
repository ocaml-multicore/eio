open Eio.Std

let n_domains = 3
let n_rounds = 1000

(* Each worker domain loops, creating resources and attaching them to the
   shared switch [sw]. It also randomly close resources, cancelling the hook.
   The main domain finishes the switch while this is happening, freeing all
   registered resources. At the end, we check that the number of resources
   allocated matches the number freed. *)
let[@warning "-52"] run_domain ~sw ~hooks resources =
  try
    while true do
      Atomic.incr resources;
      let hook = Switch.on_release_cancellable sw (fun () -> Atomic.decr resources) in
      if Random.bool () then (
        (* Manually close an existing resource. *)
        let i = Random.int (Array.length hooks) in
        if Switch.try_remove_hook hooks.(i) then
          Atomic.decr resources
      );
      if Random.bool () then (
        let i = Random.int (Array.length hooks) in
        hooks.(i) <- hook;
      )
    done
  with Invalid_argument "Switch finished!" ->
    ()

let main ~pool =
  let resources = Array.init n_domains (fun _ -> Atomic.make 0) in
  (* Keep up to 10 hooks so we can cancel them randomly too. *)
  let hooks = Array.make 10 Switch.null_hook in
  for _ = 1 to n_rounds do
    (* if i mod 1000 = 0 then traceln "Round %d" i; *)
    Switch.run (fun domains_sw ->
        Switch.run (fun sw ->
            resources |> Array.iter (fun resources ->
                Fiber.fork ~sw:domains_sw (fun () ->
                    Eio.Executor_pool.submit_exn pool ~weight:1.0 (fun () -> run_domain ~sw ~hooks resources)
                  )
              );
            (* traceln "Wait for domains to start"; *)
            while Atomic.get (resources.(n_domains - 1)) < 20 do
              Domain.cpu_relax ()
            done;
          );
        (* The child domains will start to finish as they find that
           [sw] is not accepting new resources. They may each still
           create one last resource. *)
      );
   (* All child domains are now finished. *)
    let x = Array.fold_left (fun acc resources -> acc + Atomic.get resources) 0 resources in
    if x <> 0 then Fmt.failwith "%d resources still open at end!" x
  done

let () =
  Eio_main.run @@ fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  Switch.run @@ fun sw ->
  let pool = Eio.Executor_pool.create ~sw ~domain_count:n_domains domain_mgr in
  main ~pool
