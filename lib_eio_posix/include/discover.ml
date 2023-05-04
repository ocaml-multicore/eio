module C = Configurator.V1

let optional_flags = [
  "O_DSYNC";
  "O_RESOLVE_BENEATH";
  "O_PATH";
]

let () =
  C.main ~name:"discover" (fun c ->
      let c_flags = ["-D_LARGEFILE64_SOURCE"] in
      let includes = ["sys/types.h"; "sys/stat.h"; "fcntl.h"] in
      let extra_flags, missing_defs =
        C.C_define.import c ~c_flags ~includes
          C.C_define.Type.(List.map (fun name -> name, Switch) optional_flags)
        |> List.partition_map (function
            | name, C.C_define.Value.Switch true -> Left (name, C.C_define.Type.Int)
            | name, Switch false ->
              Right (Printf.sprintf "let %s = None" (String.lowercase_ascii name))
            | _ -> assert false
          )
      in
      let present_defs =
        C.C_define.import c ~c_flags
          ~includes:["sys/types.h"; "sys/stat.h"; "fcntl.h"]
          C.C_define.Type.(extra_flags @ [
            "O_RDONLY", Int;
            "O_RDWR", Int;
            "O_WRONLY", Int;

            "O_APPEND", Int;
            "O_CLOEXEC", Int;
            "O_CREAT", Int;
            "O_DIRECTORY", Int;
            "O_EXCL", Int;
            "O_NOCTTY", Int;
            "O_NOFOLLOW", Int;
            "O_NONBLOCK", Int;
            "O_SYNC", Int;
            "O_TRUNC", Int;

            "AT_FDCWD", Int;
          ])
        |> List.map (function
            | name, C.C_define.Value.Int v when List.mem name optional_flags ->
              Printf.sprintf "let %s = Some 0x%x" (String.lowercase_ascii name) v
            | name, C.C_define.Value.Int v ->
              Printf.sprintf "let %s = 0x%x" (String.lowercase_ascii name) v
            | _ -> assert false
          )
      in
      let defs = present_defs @ missing_defs in
      C.Flags.write_lines "config.ml" defs
    )
