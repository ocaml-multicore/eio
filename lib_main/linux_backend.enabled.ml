let run ~fallback fn = Eio_linux.run ~fallback (fun env -> fn (env :> Eio.Stdenv.t))
