(* basic tests using effects *)

open Eio_linux.Low_level
open Eio.Std
module Int63 = Optint.Int63

let setup_log level =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let exec ?stdin ?stdout ?stderr ?cwd ~env ~sw prog args = 
  let stdin = Option.value ~default:(Eio_linux.get_fd env#stdin) stdin in
  let stdout = Option.value ~default:(Eio_linux.get_fd env#stdout) stdout in
  let stderr = Option.value ~default:(Eio_linux.get_fd env#stderr) stderr in
  let cwd = Option.value ~default:env#cwd cwd in
  Eio_linux.Low_level.Process.spawn ~sw ~cwd ~stdin ~stderr ~stdout prog args

let () =
  setup_log (Some Logs.Debug);
  Eio_linux.run @@ fun _stdenv ->
  Switch.run @@ fun sw ->
  let fd =
    openat2 "test.txt"
      ~sw
      ~access:`R
      ~perm:0
      ~flags:Uring.Open_flags.empty
      ~resolve:Uring.Resolve.empty
in
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
