let run ~fallback:_ fn = Eio_windows.run (fun env -> fn (env :> Eio.Stdenv.t))
