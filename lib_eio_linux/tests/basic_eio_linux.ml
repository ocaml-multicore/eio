(* basic tests using effects *)

open Eio_linux.Low_level
open Eio.Std
module Int63 = Optint.Int63

let setup_log level =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  setup_log (Some Logs.Debug);
  Eio_linux.run @@ fun _stdenv ->
  Switch.run @@ fun sw ->
  let fd = Unix.handle_unix_error (openfile ~sw "test.txt" Unix.[O_RDONLY]) 0 in
  let buf = alloc_fixed_or_wait () in
  let _ = read_exactly fd buf 5 in
  print_endline (Uring.Region.to_string ~len:5 buf);
  let _ = read_exactly fd ~file_offset:(Int63.of_int 3) buf 3 in
  print_endline (Uring.Region.to_string ~len:3 buf);
  free_fixed buf;
  (* With a sleep: *)
  let buf = alloc_fixed_or_wait () in
  let _ = read_exactly fd buf 5 in
  Logs.debug (fun l -> l "sleeping at %f" (Unix.gettimeofday ()));
  sleep_until (Mtime.add_span (Mtime_clock.now ()) Mtime.Span.s |> Option.get);
  print_endline (Uring.Region.to_string ~len:5 buf);
  let _ = read_exactly fd ~file_offset:(Int63.of_int 3) buf 3 in
  print_endline (Uring.Region.to_string ~len:3 buf);
  free_fixed buf
