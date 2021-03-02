(* basic tests using effects *)

open Euring

let setup_log level =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  setup_log (Some Logs.Debug);
  (* TODO expose openfile from euring *)
  let fd = Unix.(handle_unix_error (openfile "test.txt" [O_RDONLY]) 0) in
  run (fun () ->
    let buf = alloc () in
    let _ = read fd buf 5 in
    print_endline (Baregion.to_string ~len:5 buf);
    let _ = read fd ~file_offset:3 buf 3 in
    print_endline (Baregion.to_string ~len:3 buf);
    free buf;
  );
  run (fun () ->
    let buf = alloc () in
    let _ = read fd buf 5 in
    Logs.debug (fun l -> l "sleeping at %f" (Unix.gettimeofday ()));
    sleep 1.0;
    print_endline (Baregion.to_string ~len:5 buf);
    let _ = read fd ~file_offset:3 buf 3 in
    print_endline (Baregion.to_string ~len:3 buf);
    free buf;
  );
