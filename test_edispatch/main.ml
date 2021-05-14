let main ~stdout =
  let src = Eio.Source.of_string "Hello, world!\n" in
  Eio.Sink.write stdout ~src

let () = Edispatch.run @@ fun env -> main ~stdout:(Eio.Stdenv.stdout env)