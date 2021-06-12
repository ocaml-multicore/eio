let run fn = Main.run (fun env -> fn (env :> Eio.Stdenv.t))
