open Eio.Std

(* Stress test to check for resource leaks by repeatedly spawning fibers *)

(* Number of iterations for the stress test *)
let n_iters = 100_000

(* Main function to run the resource leak test *)
let main ~domain_mgr =
  (* Create a semaphore with a capacity of 10 *)
  let sem = Eio.Semaphore.make 10 in
  
  (* Create a new switch context for running fibers *)
  Switch.run (fun sw ->
      (* Fork a new fiber for the resource test *)
      Fiber.fork ~sw (fun () ->
          (* Run the stress test within the provided domain manager *)
          Eio.Domain_manager.run domain_mgr (fun () ->
              (* Perform the stress test iterations *)
              for _ = 1 to n_iters do
                (* Try to acquire the semaphore or yield if it's not available *)
                Fiber.first
                  (fun () -> Eio.Semaphore.acquire sem)  (* Attempt to acquire the semaphore *)
                  (fun () -> Fiber.yield ());  (* Yield if the semaphore cannot be acquired *)

                (* Release the semaphore after use *)
                Eio.Semaphore.release sem
              done
            )
        )
    );

  (* Assert that the semaphore's value is unchanged at the end of the test *)
  assert (Eio.Semaphore.get_value sem = 10);
  
  (* Print the result of the resource leak test *)
  print_endline "Resource Leak Test Passed"

(* Entry point of the program *)
let () =
  Eio_main.run @@ fun env ->
  main ~domain_mgr:(Eio.Stdenv.domain_mgr env)  (* Pass the domain manager from the environment *)
