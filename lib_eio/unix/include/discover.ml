module C = Configurator.V1

let optional_flags = [
  "ENOTCAPABLE";
]

let always_present_flags = [
]

let () =
  C.main ~name:"discover" (fun c ->
      let c_flags = [] in
      let includes = ["errno.h"] in
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
        C.C_define.import c ~c_flags ~includes (extra_flags @ always_present_flags)
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
