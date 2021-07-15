let mutex = Mutex.create ()

let default_traceln ?__POS__:pos fmt =
  let b = Buffer.create 512 in
  let k f =
    Option.iter (fun (file, lnum, _, _) -> Format.fprintf f " [%s:%d]" file lnum) pos;
    Format.pp_close_box f ();
    Format.pp_print_flush f ();
    let msg = Buffer.contents b in
    Ctf.label msg;
    let lines = String.split_on_char '\n' msg in
    Mutex.lock mutex;
    Fun.protect ~finally:(fun () -> Mutex.unlock mutex) @@ fun () ->
    List.iter (Printf.eprintf "+%s\n") lines;
    flush stderr
  in
  Format.kfprintf k (Format.formatter_of_buffer b) ("@[" ^^ fmt)
