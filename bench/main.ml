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
]

let usage_error () =
  let names = List.map fst benchmarks in
  Fmt.epr "Usage: main.exe [%a]@." Fmt.(list ~sep:(any " | ") string) names;
  exit 1

let () =
  Eio_main.run @@ fun env ->
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
    Eio.traceln "Running %s..." name;
    let metrics = fn env in
    `Assoc [
      "name", `String name;
      "metrics", `List metrics;
    ]
  in
  Fmt.pr "%a@." (Yojson.Safe.pretty_print ~std:true) @@ `Assoc [
    "results", `List (List.map run benchmarks);
  ]
