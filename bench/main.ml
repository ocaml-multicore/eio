open Eio.Std

let benchmarks = [
  "Promise", Bench_promise.run;
  "Cancel", Bench_cancel.run;
  "Buf_read", Bench_buf_read.run;
  "Condition", Bench_condition.run;
  "Fiber.yield", Bench_yield.run;
  "Mutex", Bench_mutex.run;
  "Semaphore", Bench_semaphore.run;
  "Stream", Bench_stream.run;
  "HTTP", Bench_http.run;
  "Eio_unix.Fd", Bench_fd.run;
  "File.stat", Bench_fstat.run;
  "Path.stat", Bench_stat.run;
  "Flow.copy", Bench_copy.run;
  "Eio_unix.run_in_systhread", Bench_systhread.run;
]

let usage_error () =
  let names = List.map fst benchmarks in
  Fmt.epr "Usage: main.exe [%a]@." Fmt.(list ~sep:(any " | ") string) names;
  exit 1

let () =
  Eio_main.run @@ fun env ->
  traceln "Using %s backend" env#backend_id;
  let benchmarks =
    match Array.to_list Sys.argv with
    | [_] -> benchmarks
    | [_; name] ->
      begin match List.assoc_opt name benchmarks with
        | Some run -> [name, run]
        | None ->
          Fmt.epr "Unknown benchmark %S@." name;
          usage_error ()
      end
    | _ -> usage_error ()
  in
  let run (name, fn) =
    traceln "Running %s..." name;
    let metrics = fn env in
    `Assoc [
      "name", `String name;
      "metrics", `List metrics;
    ]
  in
  (* The benchmark machine runs an old Docker that blocks pidfd_open *)
  (* let uname = Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all ["uname"; "-a"] in *)
  let uname =
    let ch = Unix.open_process_in "uname -a" in
    let x = input_line ch in
    close_in ch;
    x
  in
  Fmt.pr "%a@." (Yojson.Safe.pretty_print ~std:true) @@ `Assoc [
    "config", `Assoc [
      "uname", `String uname;
      "backend", `String env#backend_id;
      "recommended_domain_count", `Int (Domain.recommended_domain_count ());
    ];
    "results", `List (List.map run benchmarks);
  ]
