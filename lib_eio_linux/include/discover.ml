module C = Configurator.V1

let () =
  C.main ~name:"discover" (fun c ->
      let c_flags = ["-D_LARGEFILE64_SOURCE"; "-D_XOPEN_SOURCE=700"; "-D_GNU_SOURCE";] in
      let present_defs =
        C.C_define.import c ~c_flags
          ~includes:["fcntl.h"]
          C.C_define.Type.[
            "AT_SYMLINK_NOFOLLOW", Int;
          ]
        |> List.map (function
            | name, C.C_define.Value.Int v ->
              Printf.sprintf "let %s = 0x%x" (String.lowercase_ascii name) v
            | _ -> assert false
          )
      in
      let defs = present_defs in
      C.Flags.write_lines "config.ml" defs
    )
