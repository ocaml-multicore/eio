let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

open Cmdliner

let cmd =
  let setup_log =
    Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ()) in
  let infile =
    let doc = "Source filename to copy from" in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"SOURCE_FILE" ~doc) in
  let outfile =
    let doc = "Target filename to copy to" in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"TARGET_FILE" ~doc) in
  let block_size =
    let doc = "Block size per chunk in bytes" in
    Arg.(value & opt int (32 * 1024) & info ["block-size"] ~docv:"BYTES" ~doc) in
  let queue_depth =
    let doc = "Number of async requests in parallel" in
    Arg.(value & opt int 64 & info ["queue-depth"] ~docv:"ENTRIES" ~doc) in
  let doc = "copy a file using async effect-based io_uring" in
  let man =
      [
        `S "DESCRIPTION";
        `P "$(tname) copies a file using Linux io_uring.";
      ]
  in
  let info = Cmd.info "eurcp" ~version:"1.0.0" ~doc ~man in
  Cmd.v info Term.(const Eurcp_lib.run_cp $ block_size $ queue_depth $ infile $ outfile $ setup_log)
  
let () =
  match Cmd.eval cmd with
  | 0 -> exit (if Logs.err_count () > 0 then 1 else 0)
  | _ -> exit 1
