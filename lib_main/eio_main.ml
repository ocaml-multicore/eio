let run fn = Eunix.run (fun env -> fn (env :> Eio.Stdenv.t))
