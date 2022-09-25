let src = Logs.Src.create "eio_main" ~doc:"Effect-based IO main module"
module Log = (val Logs.src_log src : Logs.LOG)

let has_working_uring v =
  (* Note: if you change this, remember to change the log message below too *)
  match String.split_on_char '.' v with
  | "5" :: minor :: _ -> int_of_string minor >= 11
  | major :: _ -> int_of_string major > 5
  | [] -> false

let run_io_uring ?fallback fn =
  Log.info (fun f -> f "Selecting io-uring backend");
  Eio_linux.run ?fallback (fun env -> fn (env :> Eio.Stdenv.t))

let run_luv fn =
  Eio_luv.run (fun env -> fn (env :> Eio.Stdenv.t))

let run fn =
  match Sys.getenv_opt "EIO_BACKEND" with
  | Some "io-uring" -> run_io_uring fn
  | Some "luv" ->
    Log.info (fun f -> f "Using luv backend");
    run_luv fn
  | None | Some "" ->
    begin match Luv.System_info.uname () with
      | Ok x when has_working_uring x.release ->
        run_io_uring fn
          ~fallback:(fun (`Msg msg) ->
              Log.info (fun f -> f "%s; using luv backend instead" msg);
              run_luv fn
            )
      | _ ->
        Log.info (fun f -> f "Selecting luv backend (io-uring needs Linux >= 5.11)");
        run_luv fn
    end
  | Some x -> Fmt.failwith "Unknown eio backend %S (from $EIO_BACKEND)" x
