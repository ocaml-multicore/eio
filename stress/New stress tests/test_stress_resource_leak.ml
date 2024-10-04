open Eio.Std
open Alcotest

let test_resource_leak () =
  let n_iters = 100_000 in
  let main ~domain_mgr =
    let sem = Eio.Semaphore.make 10 in
    Switch.run (fun sw ->
        Fiber.fork ~sw (fun () ->
            Eio.Domain_manager.run domain_mgr (fun () ->
                for _ = 1 to n_iters do
                  Fiber.first
                    (fun () -> Eio.Semaphore.acquire sem)
                    (fun () -> Fiber.yield ());
                  Eio.Semaphore.release sem
                done
              )
          )
      );
    assert (Eio.Semaphore.get_value sem = 10);
    print_endline "Resource Leak Test Passed"
  in
  Eio_main.run @@ fun env ->
  main ~domain_mgr:(Eio.Stdenv.domain_mgr env)

let () =
  let open Alcotest in
  run "Stress Resource Leak Test" [
    "resource_leak", [test_case "Resource Leak Test" `Quick test_resource_leak];
  ]