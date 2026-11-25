let () =
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;
  match Array.to_list Sys.argv |> List.tl with
  | "echo" :: args -> print_endline (String.concat " " args)
  | "args" :: args -> List.iter (Printf.printf "%S\n") args
  | ["sleep"; seconds] -> Unix.sleepf (float_of_string seconds)
  | "exit" :: code :: _ -> exit (int_of_string code)
  | "sh" :: "-c" :: "exit 3" :: _ -> exit 3
  | ["sh"; "-c"; "exit 0"] -> ()
  | ["sh"; "-c"; "echo 123; exit 5"] ->
    print_endline "123";
    exit 5
  | ["cat"; path] ->
    In_channel.with_open_bin path (fun input ->
        output_string stdout (In_channel.input_all input))
  | ["wc"; "-l"] ->
    let rec count n =
      match input_line stdin with
      | _ -> count (n + 1)
      | exception End_of_file -> Printf.printf "%d\n" n
    in
    count 0
  | ["env"; "pwd"] ->
    assert (Filename.dirname (Sys.getcwd ()) = Sys.getcwd ());
    print_endline "/"
  | ["sh"; "-c"; "echo $DISPLAY"] -> print_endline (Sys.getenv "DISPLAY")
  | _ -> failwith "Unknown process test command"
