open Eio.Std

let n_rounds = 10

let main env =
  let fs = Eio.Stdenv.fs env in
  let path = Eio.Path.(fs / "/tmp/eio-test") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 path;
  Switch.run @@ fun sw ->
  Switch.on_release sw (fun () -> Eio.Path.rmtree ~missing_ok:true path);
  let num = 100 in
  traceln "-------------------------------------";
  for i = 1 to n_rounds do
    Switch.run @@ fun sw ->
    let pool = Eio.Executor_pool.create ~sw ~domain_count:5 (Eio.Stdenv.domain_mgr env) in
    List.init num Fun.id
    |> Eio.Fiber.List.iter (fun i ->
      Eio.Executor_pool.submit_exn pool ~weight:0.0 (fun () ->
        let path = Eio.Path.(path / Format.sprintf "test%d.txt" i) in
        Eio.Path.with_open_out path ~create:(`Or_truncate 0o600) @@ fun file ->
        Eio.Flow.copy_string "!!!" file
      )
    );
    traceln "Finished round %d/%d" i n_rounds
  done;
  traceln "Success"

let () =
  Eio_main.run @@ fun env ->
  main env
