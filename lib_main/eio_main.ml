let force run fn =
  run ~fallback:(fun (`Msg msg) -> failwith msg) fn

let run  ?(loc = Eio.Private.Tracing.get_caller ()) fn =
  match Sys.getenv_opt "EIO_BACKEND" with
  | Some ("io-uring" | "linux") -> force (Linux_backend.run ~loc) fn
  | Some "posix" -> force (Posix_backend.run ~loc) fn
  | Some "windows" -> force Windows_backend.run fn
  | None | Some "" ->
    Linux_backend.run ~loc fn ~fallback:(fun _ ->
        Posix_backend.run ~loc fn ~fallback:(fun _ ->
            force Windows_backend.run fn
          )
      )
  | Some x -> Fmt.failwith "Unknown Eio backend %S (from $EIO_BACKEND)" x
