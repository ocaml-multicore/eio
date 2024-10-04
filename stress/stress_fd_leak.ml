open Eio.Std

(* Stress test to check for file descriptor (FD) leaks by repeatedly opening and closing files *)

(* Number of iterations for the stress test *)
let n_iters = 100_000

(* File path to use for opening files. Using /dev/null as it doesn't create real files. *)
let file_path = "/dev/null"

(* Function to count the number of open file descriptors in the current process *)
let count_open_fds () =
  let proc_fd = "/proc/self/fd" in
  Array.length (Sys.readdir proc_fd)  (* Read the directory containing open file descriptors *)

(* Main function to run the stress test *)
let main ~domain_mgr =
  (* Count the initial number of open file descriptors *)
  let initial_fds = count_open_fds () in

  (* Create a new switch context for running fibers *)
  Switch.run (fun sw ->
      (* Fork a new fiber for the stress test *)
      Fiber.fork ~sw (fun () ->
          (* Run the stress test within the provided domain manager *)
          Eio.Domain_manager.run domain_mgr (fun () ->
              (* Perform the stress test iterations *)
              for _ = 1 to n_iters do
                let fd = Unix.openfile file_path [Unix.O_RDONLY] 0 in  (* Open the file descriptor *)
                Unix.close fd  (* Close the file descriptor *)
              done
            )
        )
    );

  (* Count the final number of open file descriptors *)
  let final_fds = count_open_fds () in

  (* Assert that the number of open file descriptors is unchanged *)
  assert (initial_fds = final_fds);
  
  (* Print the result of the FD leak test *)
  print_endline "FD Leak Test Passed"

(* Entry point of the program *)
let () =
  Eio_main.run @@ fun env ->
  main ~domain_mgr:(Eio.Stdenv.domain_mgr env)  (* Pass the domain manager from the environment *)
