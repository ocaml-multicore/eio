open Eio.Std

let ( / ) = Eio.Path.( / )

let test_eio dir =
  traceln "Using the file-system via the directory resource works:";
  let test_file = dir / "capsicum-test.txt" in
  traceln "Writing %a..." Eio.Path.pp test_file;
  Eio.Path.save test_file "A test file" ~create:(`Exclusive 0o644);
  traceln "Read: %S" (Eio.Path.load test_file);
  Eio.Path.unlink test_file
  
let test_legacy () =
  traceln "Bypassing Eio and accessing other resources should fail in Capsicum mode:";
  let ch = open_in "/etc/passwd" in 
  let len = in_channel_length ch in 
  let data = really_input_string ch len in
  close_in ch;
  traceln "Was able to read /etc/passwd:@.%s" (String.trim data)

let () =
  Eio_main.run @@ fun env ->
  (* Parse command-line arguments *)
  let path =
    match Sys.argv with
    | [| _; dir |] -> Eio.Stdenv.fs env / dir
    | _ -> failwith "Usage: main.exe DIR"
  in 
  if not (Eio.Path.is_directory path) then Fmt.failwith "%a is not a directory" Eio.Path.pp path;
  (* Get access to resources before calling cap_enter: *)
  Eio.Path.with_open_dir path @@ fun dir ->
  traceln "Opened directory %a" Eio.Path.pp path;
  (* Switch to capability mode, if possible: *)
  begin match Eio_unix.Cap.enter () with
  | Ok () -> traceln "Capsicum mode enabled"
  | Error `Not_supported -> traceln "!! CAPSICUM PROTECTION NOT AVAILABLE !!"
  end;
  (* Run tests: *)
  test_eio dir;
  test_legacy ()
