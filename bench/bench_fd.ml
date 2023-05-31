open Eio.Std

let time label len fn =
  let t0 = Unix.gettimeofday () in
  fn ();
  let t1 = Unix.gettimeofday () in
  Metric.create
    label
    (`Float (float len /. (t1 -. t0) /. (2. ** 30.))) "GB/s"
    "Reading from /dev/zero using a single FD"

let main ~domain_mgr zero =
  let iters = 100_000 in
  let len = 64 * 1024 in
  let n_fibers = 4 in
  let n_domains = 4 in
  let buf = Cstruct.create len in
  let run1 () =
    for _ = 1 to iters do Eio.Flow.read_exact zero buf done
  in
  [time "fibers:1" (iters * len) run1;
  time (Fmt.str "fibers:%d" n_fibers) (iters * n_fibers * len) (fun () ->
      Switch.run @@ fun sw ->
      for _ = 1 to n_fibers do
        Fiber.fork ~sw run1
      done
    );
  time (Fmt.str "domains:%d" n_domains) (iters * n_domains * len) (fun () ->
      Switch.run @@ fun sw ->
      for _ = 1 to n_domains do
        Fiber.fork ~sw (fun () -> Eio.Domain_manager.run domain_mgr run1)
      done
    )]

let ( / ) = Eio.Path.( / )

let run env =
  Eio.Path.with_open_in (env#fs / "/dev/zero") (main ~domain_mgr:env#domain_mgr)
