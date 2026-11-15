let () =
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;
  match Array.to_list Sys.argv with
  | _ :: "echo" :: args -> print_endline (String.concat " " args)
  | _ :: "args" :: args -> List.iter (Printf.printf "%S\n") args
  | [_; "sleep"; seconds] -> Unix.sleepf (float_of_string seconds)
  | _ :: "exit" :: code :: _ -> exit (int_of_string code)
  | [_; "echo-exit"; text; code] ->
    print_endline text;
    exit (int_of_string code)
  | [_; "cat"; path] ->
    In_channel.with_open_bin path (fun input ->
        output_string stdout (In_channel.input_all input))
  | [_; "count-lines"] ->
    let rec count n =
      match input_line stdin with
      | _ -> count (n + 1)
      | exception End_of_file -> Printf.printf "%d\n" n
    in
    count 0
  | [_; "cwd"] -> print_endline (Filename.basename (Sys.getcwd ()))
  | [_; "getenv"; name] -> print_endline (Sys.getenv name)
  | _ -> failwith "Unknown process test command"
