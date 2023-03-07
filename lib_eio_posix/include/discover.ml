module C = Configurator.V1

let () =
  C.main ~name:"discover" (fun c ->
      let defs =
        C.C_define.import c ~c_flags:["-D_LARGEFILE64_SOURCE"]
          ~includes:["sys/types.h"; "sys/stat.h"; "fcntl.h"]
          C.C_define.Type.[
            "O_RDONLY", Int;
            "O_RDWR", Int;
            "O_WRONLY", Int;

            "O_APPEND", Int;
            "O_CLOEXEC", Int;
            "O_CREAT", Int;
            "O_DIRECTORY", Int;
            "O_DSYNC", Int;
            "O_EXCL", Int;
            "O_NOCTTY", Int;
            "O_NOFOLLOW", Int;
            "O_NONBLOCK", Int;
            "O_SYNC", Int;
            "O_TRUNC", Int;

            "AT_FDCWD", Int;
          ]
        |> List.map (function
            | name, C.C_define.Value.Int v ->
              Printf.sprintf "let %s = 0x%x" (String.lowercase_ascii name) v
            | _ -> assert false
          )
      in
      C.Flags.write_lines "config.ml" defs
    )
