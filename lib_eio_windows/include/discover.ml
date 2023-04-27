module C = Configurator.V1

let () =
  C.main ~name:"discover" (fun c ->
      let defs =
        C.C_define.import c ~c_flags:["-D_LARGEFILE64_SOURCE"]
          ~includes:["sys/types.h"; "sys/stat.h"; "fcntl.h"]
          C.C_define.Type.[
            "_O_RDONLY", Int;
            "_O_RDWR", Int;
            "_O_WRONLY", Int;
            "_O_APPEND", Int;
            "_O_CREAT", Int;
            "_O_NOINHERIT", Int;
            "_O_TRUNC", Int;
            "_O_EXCL", Int;
          ]
        |> List.map (function
            | name, C.C_define.Value.Int v ->
              let name_length = String.length name in
              let name = String.sub name 1 (name_length - 1) in
              Printf.sprintf "let %s = 0x%x" (String.lowercase_ascii name) v
            | _ -> assert false
          )
      in
      C.Flags.write_lines "config.ml" defs
    )
