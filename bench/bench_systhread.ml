(* Measure the overhead of [Eio_unix.run_in_systhread]. *)

open Eio.Std

let n_iters = 1000

let do_syscall () = ignore (Unix.getuid () : int)

let work () =
  for _ = 1 to n_iters do
    Eio_unix.run_in_systhread do_syscall
  done

(* Return the average time for one call to [getuid]. *)
let run_domain ~fibers =
  let t0 = Unix.gettimeofday () in
  Switch.run ~name:"run_domain" (fun sw ->
      for _ = 1 to fibers do
        Fiber.fork ~sw work
      done
    );
  let t1 = Unix.gettimeofday () in
  (t1 -. t0) /. float n_iters

let time ~domain_mgr ~baseline ~domains ~fibers =
  let overhead t = t /. baseline in
  let name = Printf.sprintf "domains:%d fibers:%d" domains fibers in
  (* Work-around for https://github.com/ocaml/ocaml/issues/12948 *)
  let main_done, set_main_done = Promise.create () in
  Switch.run ~name @@ fun sw ->
  let times =
    List.init (domains - 1) (fun _ ->
        Fiber.fork_promise ~sw (fun () ->
            Eio.Domain_manager.run domain_mgr (fun () ->
                let r = run_domain ~fibers in
                Promise.await main_done;
                r
              )
          )
      )
  in
  let my_time = run_domain ~fibers in
  Promise.resolve set_main_done ();     (* Allow Domain.join to be called *)
  let times =
    my_time :: List.map Promise.await_exn times
    |> List.map (fun t -> t *. 1e6)
  in
  traceln "%s" name;
  times |> List.iteri (fun i t ->
      traceln "%d: %.2f us (%.1f times slower)" i t (overhead t)
    );
  let avg = (List.fold_left (+.) 0. times) /. float domains in
  Metric.create name (`Float avg) "us" name

let run env =
  let domain_mgr = env#domain_mgr in
  let baseline =
    Eio.Private.Trace.with_span "baseline" @@ fun () ->
    let t0 = Unix.gettimeofday () in
    for _ = 1 to n_iters do
      do_syscall ()
    done;
    let t1 = Unix.gettimeofday () in
    ((t1 -. t0) /. float n_iters) *. 1e6
  in
  traceln "baseline (no systhreads): %.2f us" baseline;
  let results =
    [
      time ~domains:1 ~fibers:1;
      time ~domains:1 ~fibers:2;
      time ~domains:1 ~fibers:4;
      time ~domains:4 ~fibers:1;
    ]
    |> List.map (fun f -> f ~domain_mgr ~baseline)
  in
  Metric.create "blocking" (`Float baseline) "us" "baseline" :: results
