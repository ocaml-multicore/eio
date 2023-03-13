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
  Sys.set_signal Sys.sigint old;;
+interrupted!
- : unit = ()
```
