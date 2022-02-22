open Eio.Std

(* Three domains fighting over 2 semaphore tokens,
   with domains cancelling if they don't get served quickly. *)

let n_domains = 3
let n_tokens = 2
let n_iters = 100_000

let main ~domain_mgr =
  let sem = Eio.Semaphore.make n_tokens in
  Switch.run (fun sw ->
      for _ = 1 to n_domains do
        Fiber.fork ~sw (fun () ->
            Eio.Domain_manager.run domain_mgr (fun () ->
                let i = ref 0 in
                while !i < n_iters do
                  let got = ref false in
                  Fiber.first
                    (fun () -> Eio.Semaphore.acquire sem; got := true)
                    (fun () -> Fiber.yield ());
                  if !got then (
                    incr i;
                    Eio.Semaphore.release sem;
                  ) else (
                    (* traceln "yield" *)
                  )
                done
              )
          )
      done;
    );
  assert (Eio.Semaphore.get_value sem = n_tokens);
  print_endline "OK"

let () =
  Eio_main.run @@ fun env ->
  main ~domain_mgr:(Eio.Stdenv.domain_mgr env)
