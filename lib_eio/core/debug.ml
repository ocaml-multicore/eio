type traceln = {
  traceln : 'a. ?__POS__:string * int * int * int -> ('a, Format.formatter, unit, unit) format4 -> 'a;
} [@@unboxed]

let traceln_key : traceln Fiber.key = Fiber.create_key ()

let traceln_mutex = Mutex.create ()

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
    Mutex.lock traceln_mutex;
    Fun.protect ~finally:(fun () -> Mutex.unlock traceln_mutex) @@ fun () ->
    List.iter (Printf.eprintf "+%s\n") lines;
    flush stderr
  in
  Format.kdprintf k ("@[" ^^ fmt)

let traceln ?__POS__ fmt =
  let traceln =
    match Fiber.get traceln_key with
    | Some { traceln } -> traceln
    | None
    | exception (Effect.Unhandled _) -> default_traceln
  in
  traceln ?__POS__ fmt

type t = <
  traceln : traceln Fiber.key;
>

let v = object
  method traceln = traceln_key
end
