module C = Configurator.V1

let () =
  C.main ~name:"discover" (fun c ->
      let defs =
        C.C_define.import c ~c_flags:["-D_LARGEFILE64_SOURCE"]
          ~includes:["sys/types.h"; "sys/stat.h"; "fcntl.h"; "winternl.h"; "ntdef.h"]
          C.C_define.Type.[
            "_O_RDONLY", Int;
            "_O_RDWR", Int;
            "_O_WRONLY", Int;
            "_O_APPEND", Int;
            "_O_CREAT", Int;
            "_O_NOINHERIT", Int;
            "_O_TRUNC", Int;
            "_O_EXCL", Int;

            (* Desired Access *)
            "GENERIC_READ", Int;
            "GENERIC_WRITE", Int;
            "SYNCHRONIZE", Int;
            "FILE_APPEND_DATA", Int;

            (* Create Disposition *)
            "FILE_SUPERSEDE", Int;
            "FILE_CREATE", Int;
            "FILE_OPEN", Int;
            "FILE_OPEN_IF", Int;
            "FILE_OVERWRITE", Int;
            "FILE_OVERWRITE_IF", Int;

            (* Create Options *)
            "FILE_DIRECTORY_FILE", Int;
            "FILE_NON_DIRECTORY_FILE", Int;
            "FILE_NO_INTERMEDIATE_BUFFERING", Int;
            "FILE_WRITE_THROUGH", Int;
            "FILE_SEQUENTIAL_ONLY", Int;
          ]
        |> List.map (function
            | name, C.C_define.Value.Int v ->
              let name =
                if name.[0] = '_' then 
                  let name_length = String.length name in
                  String.sub name 1 (name_length - 1)
                else name
              in
              Printf.sprintf "let %s = 0x%x" (String.lowercase_ascii name) v
            | _ -> assert false
          )
      in
      C.Flags.write_lines "config.ml" defs
    )
