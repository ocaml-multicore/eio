module C = Configurator.V1

let is_windows t =
  match C.ocaml_config_var_exn t "os_type" with
  | "Win32" -> true
  | _ -> false

let is_posix t =
  match C.ocaml_config_var_exn t "os_type" with
  | "Unix" -> true
  | _ -> false

let () =
  C.main ~name:"discover" (fun c ->
    C.C_define.gen_header_file c ~fname:"eio_config.h"
    ["EIO_ON_WINDOWS", Switch (is_windows c);
     "EIO_ON_POSIX", Switch (is_posix c)])
