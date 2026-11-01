let run ~config ~fallback fn =
  Eio_linux.run ~config ~fallback (fun env -> fn (env :> Eio_unix.Stdenv.base))
