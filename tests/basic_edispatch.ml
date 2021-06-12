open Fibreslib 

let main _env =
  Switch.top @@ fun sw ->
  Fibre.both ~sw
    (fun () -> for x = 1 to 3 do traceln "x = %d" x; Fibre.yield ~sw () done)
    (fun () -> for y = 1 to 3 do traceln "y = %d" y; Fibre.yield ~sw () done)

let sim_err _env =
  Switch.top @@ fun sw ->
  Fibre.both ~sw
    (fun () -> for x = 1 to 3 do traceln "x = %d" x; Fibre.yield ~sw () done)
    (fun () -> failwith "Simulated error")

let hello ~stdout =
  let src = Eio.Flow.string_source "Hello, world!\n" in
  Eio.Flow.write stdout ~src

let print_file () =
  let infd = Edispatch.openfile "README.md" [ O_RDONLY ] 0 in
  let stdout = Edispatch.FD.of_unix (Unix.stdout) in 
  let data = ref @@ Dispatch.Data.empty () in 
  try
    while true do  
      let _res = Edispatch.read_upto ~file_offset:0 infd data max_int in
      Edispatch.write ~file_offset:0 stdout data max_int
    done
  with End_of_file -> Edispatch.write ~file_offset:0 stdout data max_int

let () = 
  Edispatch.run main;
  Edispatch.run @@ fun env -> hello ~stdout:(Eio.Stdenv.stdout env);
  Edispatch.run @@ fun _env -> print_file ()