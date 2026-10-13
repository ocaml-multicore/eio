(* This test runs two domains. One acquires a semaphore and sends a signal,
   which either domain may receive. The handler releases the semaphore, allowing
   the next signal to be sent.

   If [use_fix = true] then this uses [Eio_unix.set_signal], which should work
   correctly. Without it, it uses [Sys.set_signal] which will typically hang at
   some point.

   On my machine:

   eio_linux  use_fix=false  hung on 4/5 runs
   eio_posix  use_fix=false  hung on 3/5 runs

   Both passed every time with use_fix=true. *)

open Eio.Std

let limit = 1000000
let use_fix = true

let set_signal =
  if use_fix then Eio_unix.set_signal
  else Sys.set_signal

let run_signalling_domain sem =
  let pid = Unix.getpid () in
  while true do
    Eio.Semaphore.acquire sem;
    Fiber.check ();
    Unix.kill pid Sys.sighup;
    for _ = 0 to Random.int 1000 do () done
  done

let () =
  Eio_main.run @@ fun env ->
  traceln "Testing %d signals with use_fix=%b (%s)" limit use_fix env#backend_id;
  Switch.run @@ fun sw ->
  let finished, set_finished = Promise.create () in
  let sem = Eio.Semaphore.make 1 in
  let i = ref 0 in
  let handle _ =
    let v = !i in
    i := v + 1;
    if v mod 10000 = 0 then traceln "Got signal %d" v;
    if v = limit then Promise.resolve set_finished ()
    else Eio.Semaphore.release sem;
    for _ = 0 to Random.int 1000 do () done
  in
  set_signal Sys.sighup (Signal_handle handle);
  Fiber.fork_daemon ~sw (fun () ->
      Eio.Domain_manager.run env#domain_mgr (fun () -> run_signalling_domain sem);
    );
  Promise.await finished;
  traceln "Pass"
