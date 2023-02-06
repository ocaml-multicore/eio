open Eio.Std

let load_config () =
  (* A real system would load the file and then pass it to the running service
     somehow, but we're just demonstrating signal handling so just sleep to
     simulate some time taken to load the new configuration. *)
  Eio_unix.sleep 2.0

(* $MDX part-begin=main *)
let main ~config_changed =
  while true do
    Fiber.both
      (fun () ->
         (* First, we start waiting for SIGHUP.
            This is so that if we get SIGHUP before we finish loading
            the old configuration then we'll start again. *)
         Eio.Condition.await_no_mutex config_changed;
         traceln "Received SIGHUP";
         (* We could cancel the loading fiber now, in case it's still running,
            but in this example we just wait for it to finish by itself. *)
      )
      (fun () ->
         traceln "Reading configuration ('kill -SIGHUP %d' to reload)..." (Unix.getpid ());
         load_config ();
         traceln "Finished reading configuration";
      )
  done
(* $MDX part-end *)

let () =
  Eio_main.run @@ fun _env ->
  let config_changed = Eio.Condition.create () in
  let handle_signal (_signum : int) =
    (* Warning: we're in a signal handler now.
       Most operations are unsafe here, except for Eio.Condition.broadcast! *)
    Eio.Condition.broadcast config_changed
  in
  Sys.set_signal Sys.sighup (Signal_handle handle_signal);
  main ~config_changed
