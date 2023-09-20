open Eio.Std

let ( / ) = Eio.Path.( / )

let n_stat = 100000

let run_fiber file =
  for _ = 1 to n_stat do
    let info = (Eio.File.stat file).kind in
    assert (info = `Regular_file)
  done

let run env =
  Eio.Path.with_open_out ~create:(`If_missing 0o600) (env#cwd / "test-stat") @@ fun file ->
  [1; 10] |> List.map (fun par ->
      let t0 = Unix.gettimeofday () in
      Switch.run (fun sw ->
          for _  = 1 to par do
            Fiber.fork ~sw (fun () -> run_fiber file)
          done
        );
      let t1 = Unix.gettimeofday () in
      let stat_per_s = float (n_stat * par) /. (t1 -. t0) in
      let label = Printf.sprintf "n=%d fibers=%d" n_stat par in
      Metric.create label (`Float stat_per_s) "stat/s" "Call fstat on an open file"
    )
