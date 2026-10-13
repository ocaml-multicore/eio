# Setting up the environment

```ocaml
# #require "eio_main";;
# open Eio.Std;;
```

# Test cases

Prove we can catch sigint:
```ocaml
# Eio_main.run @@ fun _stdenv ->
  let interrupted = Eio.Condition.create () in
  let old = Sys.signal Sys.sigint
      (Signal_handle (fun num -> if num = Sys.sigint then Eio.Condition.broadcast interrupted))
  in
  Fiber.both
    (fun () ->
      Eio.Condition.await_no_mutex interrupted;
      traceln "interrupted!";
    )
    (fun () ->
      let ppid = Unix.getpid () in
      match Unix.fork () with
      | 0 ->
        Unix.kill ppid Sys.sigint;
        Unix._exit 0
      | child_pid ->
        let rec wait () =
          match Unix.waitpid [] child_pid with
          | pid, status ->
            assert (pid = child_pid);
            assert (status = (Unix.WEXITED 0))
          | exception Unix.Unix_error (EINTR, _, _) -> wait ()
          | exception Unix.Unix_error (ECHILD, _, _) -> ()  (* Hack until we have a cross-platform process API *)
        in
        wait ()
    );
  Eio_unix.set_signal Sys.sigint old;;
+interrupted!
- : unit = ()
```

Check that the FD wake-up mechanism works:

```ocaml
module Signal_fd = Eio_unix.Private.Signal_fd

(* Check whether the signal FD says a signal needs handling. *)
let signal_ready () =
  let fd = (Eio_unix.Private.Signal_fd.get_singleton () :> Unix.file_descr) in
  let r, _, _ = Unix.select [fd] [] [] 0.0 in
  r <> []

let rec await_signal_ready fd =
  match Unix.select [fd] [] [] 120.0 with
  | exception Unix.Unix_error (EINTR, _, _) -> await_signal_ready fd
  | [_], _, _ -> () 
  | _ -> failwith "Timeout waiting for signal"
```

```ocaml
# Eio_unix.set_signal Sys.sigint (Signal_handle ignore);
  let fd = Signal_fd.get_singleton () in
  Signal_fd.ack fd;     (* Ensure clear to start *)
  let thread = Thread.create (fun () -> Unix.kill (Unix.getpid ()) Sys.sigint) () in
  await_signal_ready (fd :> Unix.file_descr);
  traceln "Signal FD is ready for reading";
  Thread.join thread;
  while signal_ready () do Signal_fd.ack fd done;
  traceln "Signal FD cleared";;
+Signal FD is ready for reading
+Signal FD cleared
- : unit = ()
```
