module Ctf = Eio.Private.Ctf

let mutex = Mutex.create ()

let default_traceln ?__POS__:pos fmt =
  let k go =
    let b = Buffer.create 512 in
    let f = Format.formatter_of_buffer b in
    go f;
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
  Format.kdprintf k ("@[" ^^ fmt)
