let has_working_uring v =
  match String.split_on_char '.' v with
  | "5" :: minor :: _ -> int_of_string minor >= 10
  | major :: _ -> int_of_string major > 5
  | [] -> false

let run_io_uring ?fallback fn =
  Logs.info (fun f -> f "Selecting io-uring backend");
  Eio_linux.run ?fallback (fun env -> fn (env :> Eio.Stdenv.t))

let run_luv fn =
  Eio_luv.run (fun env -> fn (env :> Eio.Stdenv.t))

let run fn =
  match Sys.getenv_opt "EIO_BACKEND" with
  | Some "io-uring" -> run_io_uring fn
  | Some "luv" ->
    Logs.info (fun f -> f "Using luv backend");
    run_luv fn
  | None | Some "" ->
    begin match Luv.System_info.uname () with
      | Ok x when has_working_uring x.release ->
        run_io_uring fn
          ~fallback:(fun (`Msg msg) ->
              Logs.info (fun f -> f "%s; using luv backend instead" msg);
              run_luv fn
            )
      | _ ->
        Logs.info (fun f -> f "Selecting luv backend (io-uring needs Linux >= 5.10)");
        run_luv fn
    end
  | Some x -> Fmt.failwith "Unknown eio backend %S (from $EIO_BACKEND)" x
