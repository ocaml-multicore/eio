# Setting up the environment

```ocaml
# #require "eio.mock";;
```

Creating some useful helper functions

```ocaml
open Eio.Std

module Executor_pool = Eio.Executor_pool

let () = Eio.Exn.Backend.show := false

let run fn =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio_mock.Domain_manager.run @@ fun mgr ->
  let clock = Eio.Stdenv.clock env in
  let sleep ?weight ms =
    let t0 = Eio.Time.now clock in
    let t1 = t0 +. ms in
    let w_info = weight |> Option.map (Format.sprintf " (weight: %.03f)") |> Option.value ~default:"" in
    traceln "Sleeping %.0f%s: %.0f -> %.0f" ms w_info t0 t1;
    Eio.Time.sleep clock ms
  in
  let duration expected f =
    let t0 = Eio.Time.now clock in
    let res = f () in
    let t1 = Eio.Time.now clock in
    let actual = t1 -. t0 in
    if Float.(actual = expected)
    then (traceln "Duration (valid): %.0f" expected; res)
    else Fmt.failwith "Duration was not %.0f: %.0f" expected actual
  in
  fn mgr sleep duration
```

# Concurrency

Runs jobs in parallel as much as possible (domains):

```ocaml
# run @@ fun mgr sleep duration ->
  Switch.run @@ fun sw ->
  let total = ref 0 in
  let pool = Executor_pool.create ~sw ~domain_count:2 mgr in
  duration 150. (fun () ->
    List.init 5 (fun i -> i + 1)
    |> Fiber.List.iter (fun i -> Executor_pool.submit_exn pool ~weight:1. (fun () ->
      sleep 50.;
      total := !total + i
    ));
    !total
  );;
+[1] Sleeping 50: 0 -> 50
+[2] Sleeping 50: 0 -> 50
+mock time is now 50
+[1] Sleeping 50: 50 -> 100
+[2] Sleeping 50: 50 -> 100
+mock time is now 100
+[1] Sleeping 50: 100 -> 150
+mock time is now 150
+[0] Duration (valid): 150
- : int = 15
```

Runs jobs in parallel as much as possible (workers):

```ocaml
# run @@ fun mgr sleep duration ->
  Switch.run @@ fun sw ->
  let total = ref 0 in
  let pool = Executor_pool.create ~sw ~domain_count:1 mgr in
  duration 150. (fun () ->
    List.init 5 (fun i -> i + 1)
    |> Fiber.List.iter (fun i -> Executor_pool.submit_exn pool ~weight:0.5 (fun () ->
      sleep 50.;
      total := !total + i
    ));
    !total
  );;
+[1] Sleeping 50: 0 -> 50
+[1] Sleeping 50: 0 -> 50
+mock time is now 50
+[1] Sleeping 50: 50 -> 100
+[1] Sleeping 50: 50 -> 100
+mock time is now 100
+[1] Sleeping 50: 100 -> 150
+mock time is now 150
+[0] Duration (valid): 150
- : int = 15
```

Runs jobs in parallel as much as possible (both):

```ocaml
# run @@ fun mgr sleep duration ->
  Switch.run @@ fun sw ->
  let total = ref 0 in
  let pool = Executor_pool.create ~sw ~domain_count:2 mgr in
  duration 100. (fun () ->
    List.init 5 (fun i -> i + 1)
    |> Fiber.List.iter (fun i -> Executor_pool.submit_exn pool ~weight:0.5 (fun () ->
      sleep 50.;
      total := !total + i
    ));
    !total
  );;
+[1] Sleeping 50: 0 -> 50
+[2] Sleeping 50: 0 -> 50
+[1] Sleeping 50: 0 -> 50
+[2] Sleeping 50: 0 -> 50
+mock time is now 50
+[1] Sleeping 50: 50 -> 100
+mock time is now 100
+[0] Duration (valid): 100
- : int = 15
```

Can exceed weight of 1.0:

```ocaml
# run @@ fun mgr sleep duration ->
  Switch.run @@ fun sw ->
  let pool = Executor_pool.create ~sw ~domain_count:2 mgr in
  [
    List.init 10 (fun _ -> 0.25);
    [ 1.0 ];
    List.init 7 (fun _ -> 0.25);
  ]
  |> List.concat
  |> Fiber.List.iter (fun weight ->
    Executor_pool.submit_exn pool ~weight (fun () -> sleep ~weight 100.)
  )
  ;;
+[1] Sleeping 100 (weight: 0.250): 0 -> 100
+[2] Sleeping 100 (weight: 0.250): 0 -> 100
+[1] Sleeping 100 (weight: 0.250): 0 -> 100
+[2] Sleeping 100 (weight: 0.250): 0 -> 100
+[1] Sleeping 100 (weight: 0.250): 0 -> 100
+[2] Sleeping 100 (weight: 0.250): 0 -> 100
+[1] Sleeping 100 (weight: 0.250): 0 -> 100
+[2] Sleeping 100 (weight: 0.250): 0 -> 100
+mock time is now 100
+[1] Sleeping 100 (weight: 0.250): 100 -> 200
+[2] Sleeping 100 (weight: 0.250): 100 -> 200
+[1] Sleeping 100 (weight: 1.000): 100 -> 200
+[2] Sleeping 100 (weight: 0.250): 100 -> 200
+[2] Sleeping 100 (weight: 0.250): 100 -> 200
+[2] Sleeping 100 (weight: 0.250): 100 -> 200
+mock time is now 200
+[1] Sleeping 100 (weight: 0.250): 200 -> 300
+[2] Sleeping 100 (weight: 0.250): 200 -> 300
+[1] Sleeping 100 (weight: 0.250): 200 -> 300
+[2] Sleeping 100 (weight: 0.250): 200 -> 300
+mock time is now 300
- : unit = ()
```

# Weights

Must be between 0 and 1:

```ocaml
# run @@ fun mgr sleep duration ->
  Switch.run @@ fun sw ->
  let pool = Executor_pool.create ~sw ~domain_count:2 mgr in
  Executor_pool.submit_exn pool ~weight:(-5.) (fun () -> ())
  ;;
Exception: Invalid_argument "Executor_pool: weight -5 not >= 0.0 && <= 1.0".
```
```ocaml
# run @@ fun mgr sleep duration ->
  Switch.run @@ fun sw ->
  let pool = Executor_pool.create ~sw ~domain_count:2 mgr in
  Executor_pool.submit_exn pool ~weight:1.1 (fun () -> ())
  ;;
Exception: Invalid_argument "Executor_pool: weight 1.1 not >= 0.0 && <= 1.0".
```


# Job error handling

`Executor_pool.submit` returns a Result:

```ocaml
# run @@ fun mgr sleep duration ->
  Switch.run @@ fun sw ->
  let total = ref 0 in
  let pool = Executor_pool.create ~sw ~domain_count:1 mgr in
  duration 100. (fun () ->
    let results =
      List.init 5 (fun i -> i + 1)
      |> Fiber.List.map (fun i -> Executor_pool.submit pool ~weight:0.25 (fun () ->
        sleep 50.;
        if i mod 2 = 0
        then failwith (Int.to_string i)
        else (let x = !total in total := !total + i; x)
    ))
    in
    results, !total
  );;
+[1] Sleeping 50: 0 -> 50
+[1] Sleeping 50: 0 -> 50
+[1] Sleeping 50: 0 -> 50
+[1] Sleeping 50: 0 -> 50
+mock time is now 50
+[1] Sleeping 50: 50 -> 100
+mock time is now 100
+[0] Duration (valid): 100
- : (int, exn) result list * int =
([Ok 0; Error (Failure "2"); Ok 1; Error (Failure "4"); Ok 4], 9)
```

`Executor_pool.submit_exn` raises:

```ocaml
# run @@ fun mgr sleep duration ->
  Switch.run @@ fun sw ->
  let total = ref 0 in
  let pool = Executor_pool.create ~sw ~domain_count:1 mgr in
  List.init 5 (fun i -> i + 1)
  |> Fiber.List.map (fun i -> Executor_pool.submit_exn pool ~weight:1. (fun () ->
    traceln "Started %d" i;
    let x = !total in
    total := !total + i;
    if x = 3
    then failwith (Int.to_string i)
    else x
  ));;
+[1] Started 1
+[1] Started 2
+[1] Started 3
Exception: Failure "3".
```

# Blocking for capacity

`Executor_pool.submit_exn` will block waiting for room in the queue:

```ocaml
# run @@ fun mgr sleep duration ->
  Switch.run @@ fun sw ->
  let pool = Executor_pool.create ~sw ~domain_count:1 mgr in

  let p1 = Fiber.fork_promise ~sw (fun () -> Executor_pool.submit_exn pool ~weight:1. (fun () -> sleep 50.)) in

  duration 50. (fun () -> Executor_pool.submit_exn pool ~weight:1. @@ fun () -> ());

  duration 0. (fun () -> Promise.await_exn p1)
  ;;
+[1] Sleeping 50: 0 -> 50
+mock time is now 50
+[0] Duration (valid): 50
+[0] Duration (valid): 0
- : unit = ()
```

`Executor_pool.submit_fork` will not block if there's not enough room in the queue:

```ocaml
# run @@ fun mgr sleep duration ->
  Switch.run @@ fun sw ->
  let pool = Executor_pool.create ~sw ~domain_count:1 mgr in

  let p1 = duration 0. (fun () ->
    Fiber.fork_promise ~sw (fun () -> Executor_pool.submit_exn pool ~weight:1. (fun () -> sleep 50.))
  )
  in
  let p2 = duration 0. (fun () ->
    Fiber.fork_promise ~sw (fun () -> Executor_pool.submit_exn pool ~weight:1. (fun () -> sleep 50.))
  )
  in
  let p3 = duration 0. (fun () ->
    Executor_pool.submit_fork ~sw pool ~weight:1. (fun () -> ())
  )
  in

  duration 100. (fun () ->
    Promise.await_exn p1;
    Promise.await_exn p2;
    Promise.await_exn p3;
    (* Value restriction :( *)
    Promise.create_resolved (Ok ())
  )
  |> Promise.await_exn
  ;;
+[0] Duration (valid): 0
+[0] Duration (valid): 0
+[0] Duration (valid): 0
+[1] Sleeping 50: 0 -> 50
+mock time is now 50
+[1] Sleeping 50: 50 -> 100
+mock time is now 100
+[0] Duration (valid): 100
- : unit = ()
```

# Checks switch status

```ocaml
# run @@ fun mgr sleep duration ->
  let leak = ref None in
  let count = ref 0 in

  let () =
    try (
      Switch.run @@ fun sw ->

      let pool = Executor_pool.create ~sw ~domain_count:1 mgr in
      leak := Some pool;

      let p1 = duration 0. (fun () ->
        Fiber.fork_promise ~sw (fun () -> Executor_pool.submit_exn pool ~weight:1. (fun () -> sleep 50.; incr count))
      )
      in
      Switch.fail sw (Failure "Abort mission!");
      Promise.await_exn p1;
      Executor_pool.submit_exn pool ~weight:1. (fun () -> sleep 50.; incr count) )
    with _ -> ()
  in
  match !leak with
  | None -> assert false
  | Some pool ->
    Executor_pool.submit_exn pool ~weight:1. (fun () -> sleep 50.; incr count);
    traceln "Count: %d" !count
+[0] Duration (valid): 0
Exception: Invalid_argument "Stream closed".
```

If the worker is cancelled, the client still gets a reply:

```ocaml
# run @@ fun mgr sleep duration ->
  Switch.run @@ fun sw ->
  let p = Executor_pool.create ~sw ~domain_count:1 mgr in
  Fiber.both
    (fun () ->
       Eio.Cancel.protect @@ fun () ->
       traceln "Submitting...";
       Executor_pool.submit_exn p ~weight:1. (fun () -> traceln "Running");
       assert false
    )
    (fun () -> traceln "Simulated error"; Switch.fail sw Exit)
+[0] Submitting...
+[0] Simulated error
Exception: Stdlib.Exit.
```
