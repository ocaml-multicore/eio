# Setting up the environment

```ocaml
# #require "eio_main";;
# #require "eio.mock";;
```

Creating some useful helper functions

```ocaml
open Eio.Std

module Workpool = Eio.Workpool

let () = Eio.Exn.Backend.show := false

let run fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_mock.Domain_manager.run @@ fun mgr ->
  let clock = Eio_mock.Clock.make () in
  let sleep ms =
    let t0 = Eio.Time.now clock in
    let t1 = t0 +. ms in
    traceln "Sleeping %.0f: %.0f -> %.0f" ms t0 t1;
    Fiber.both
      (fun () -> Eio.Time.sleep_until clock t1)
      (fun () ->
        Fiber.yield ();
        Fiber.yield ();
        Fiber.yield ();
        Fiber.yield ();
        Fiber.yield ();
        Fiber.yield ();
        Fiber.yield ();
        if Float.(Eio.Time.now clock <> t1) then
        Eio_mock.Clock.advance clock)
  in
  let duration expected f =
    let t0 = Eio.Time.now clock in
    let res = f () in
    let t1 = Eio.Time.now clock in
    let actual = t1 -. t0 in
    if Float.(actual = expected)
    then (traceln "Duration (valid): %.0f" expected; res)
    else failwith (Format.sprintf "Duration was not %.0f: %.0f" expected actual)
  in
  fn mgr sleep duration
```

# Workpool.create

Workpool is created, transient by default:

```ocaml
# run @@ fun mgr sleep duration ->
  Switch.run @@ fun sw ->
  ignore @@ Workpool.create
    ~sw ~domain_count:2 ~domain_concurrency:1 mgr
  ;;
- : unit = ()
```

Workpool holds up the switch when non-transient:

```ocaml
# run @@ fun mgr sleep duration ->
  let terminated = ref false in
  Switch.run (fun sw ->
    let wp =
      Workpool.create
        ~sw ~domain_count:2 ~domain_concurrency:1 mgr ~transient:false
    in
    Fiber.fork_daemon ~sw (fun () ->
      Fiber.yield ();
      terminated := true;
      Workpool.terminate wp;
      `Stop_daemon
    )
  );
  !terminated
  ;;
- : bool = true
```

# Concurrency

Runs jobs in parallel as much as possible (domains):

```ocaml
# run @@ fun mgr sleep duration ->
  Switch.run @@ fun sw ->
  let total = ref 0 in
  let wp = Workpool.create ~sw ~domain_count:2 ~domain_concurrency:1 mgr in
  duration 150. (fun () ->
    List.init 5 (fun i -> i + 1)
    |> Fiber.List.iter (fun i -> Workpool.submit_exn wp (fun () ->
      sleep 50.;
      total := !total + i
    ));
    !total
  );;
+[1] Sleeping 50: 0 -> 50
+[2] Sleeping 50: 0 -> 50
+[1] mock time is now 50
+[1] Sleeping 50: 50 -> 100
+[2] Sleeping 50: 50 -> 100
+[1] mock time is now 100
+[1] Sleeping 50: 100 -> 150
+[1] mock time is now 150
+[0] Duration (valid): 150
- : int = 15
```

Runs jobs in parallel as much as possible (workers):

```ocaml
# run @@ fun mgr sleep duration ->
  Switch.run @@ fun sw ->
  let total = ref 0 in
  let wp = Workpool.create ~sw ~domain_count:1 ~domain_concurrency:2 mgr in
  duration 150. (fun () ->
    List.init 5 (fun i -> i + 1)
    |> Fiber.List.iter (fun i -> Workpool.submit_exn wp (fun () ->
      sleep 50.;
      total := !total + i
    ));
    !total
  );;
+[1] Sleeping 50: 0 -> 50
+[1] Sleeping 50: 0 -> 50
+[1] mock time is now 50
+[1] Sleeping 50: 50 -> 100
+[1] Sleeping 50: 50 -> 100
+[1] mock time is now 100
+[1] Sleeping 50: 100 -> 150
+[1] mock time is now 150
+[0] Duration (valid): 150
- : int = 15
```

Runs jobs in parallel as much as possible (both):

```ocaml
# run @@ fun mgr sleep duration ->
  Switch.run @@ fun sw ->
  let total = ref 0 in
  let wp = Workpool.create ~sw ~domain_count:2 ~domain_concurrency:2 mgr in
  duration 100. (fun () ->
    List.init 5 (fun i -> i + 1)
    |> Fiber.List.iter (fun i -> Workpool.submit_exn wp (fun () ->
      sleep 50.;
      total := !total + i
    ));
    !total
  );;
+[1] Sleeping 50: 0 -> 50
+[2] Sleeping 50: 0 -> 50
+[1] Sleeping 50: 0 -> 50
+[2] Sleeping 50: 0 -> 50
+[1] mock time is now 50
+[1] Sleeping 50: 50 -> 100
+[1] mock time is now 100
+[0] Duration (valid): 100
- : int = 15
```

# Job error handling

`Workpool.submit` returns a Result:

```ocaml
# run @@ fun mgr sleep duration ->
  Switch.run @@ fun sw ->
  let total = ref 0 in
  let wp = Workpool.create ~sw ~domain_count:1 ~domain_concurrency:4 mgr in
  duration 100. (fun () ->
    let results =
      List.init 5 (fun i -> i + 1)
      |> Fiber.List.map (fun i -> Workpool.submit wp (fun () ->
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
+[1] mock time is now 50
+[1] Sleeping 50: 50 -> 100
+[1] mock time is now 100
+[0] Duration (valid): 100
- : (int, exn) result list * int =
([Ok 0; Error (Failure "2"); Ok 1; Error (Failure "4"); Ok 4], 9)
```

`Workpool.submit_exn` raises:

```ocaml
# run @@ fun mgr sleep duration ->
  Switch.run @@ fun sw ->
  let total = ref 0 in
  let wp = Workpool.create ~sw ~domain_count:1 ~domain_concurrency:2 mgr in
  List.init 5 (fun i -> i + 1)
  |> Fiber.List.map (fun i -> Workpool.submit_exn wp (fun () ->
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
+[1] Started 4
Exception: Failure "3".
```

# Blocking for capacity

`Workpool.submit` will block waiting for room in the queue:

```ocaml
# run @@ fun mgr sleep duration ->
  Switch.run @@ fun sw ->
  let wp = Workpool.create ~sw ~domain_count:1 ~domain_concurrency:1 mgr in

  let p1 = Fiber.fork_promise ~sw (fun () -> Workpool.submit_exn wp (fun () -> sleep 50.)) in

  duration 50. (fun () -> Workpool.submit_exn wp @@ fun () -> ());

  duration 0. (fun () -> Promise.await_exn p1)
  ;;
+[1] Sleeping 50: 0 -> 50
+[1] mock time is now 50
+[0] Duration (valid): 50
+[0] Duration (valid): 0
- : unit = ()
```

`Workpool.submit_fork` will not block if there's not enough room in the queue:

```ocaml
# run @@ fun mgr sleep duration ->
  Switch.run @@ fun sw ->
  let wp = Workpool.create ~sw ~domain_count:1 ~domain_concurrency:1 mgr in

  let p1 = duration 0. (fun () ->
    Fiber.fork_promise ~sw (fun () -> Workpool.submit_exn wp (fun () -> sleep 50.))
  )
  in
  let p2 = duration 0. (fun () ->
    Fiber.fork_promise ~sw (fun () -> Workpool.submit_exn wp (fun () -> sleep 50.))
  )
  in
  let p3 = duration 0. (fun () ->
    Workpool.submit_fork ~sw wp (fun () -> ())
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
+[1] mock time is now 50
+[1] Sleeping 50: 50 -> 100
+[1] mock time is now 100
+[0] Duration (valid): 100
- : unit = ()
```

# Termination

`Workpool.terminate` waits for jobs currently running to finish and rejects queued jobs:

```ocaml
# run @@ fun mgr sleep duration ->
  let print_status wp =
    traceln "Terminating: %b (terminated: %b)"
      (Workpool.is_terminating wp) (Workpool.is_terminated wp)
  in
  Switch.run @@ fun sw ->
  let total = ref 0 in
  let wp = Workpool.create ~sw ~domain_count:2 ~domain_concurrency:2 mgr in
  let results = Fiber.fork_promise ~sw (fun () ->
    duration 300. (fun () ->
      List.init 5 (fun i -> i + 1)
      |> Fiber.List.iter (fun i -> Workpool.submit_exn wp (fun () ->
        sleep 150.;
        total := !total + i
      ));
      !total
    )
  )
  in
  sleep 75.;
  (* Exactly one job should be left in the queue
     for Workpool.terminate to reject *)
  let x = duration 75. (fun () ->
    print_status wp;
    let p = Fiber.fork_promise ~sw (fun () -> Workpool.terminate wp) in
    Fiber.fork_daemon ~sw (fun () ->
      Workpool.submit wp (fun () -> ())
      |> Result.is_error
      |> traceln "after_terminate is_error: %b";
      `Stop_daemon
    );
    print_status wp;
    Promise.await_exn p;
    print_status wp;
    !total
  )
  in
  traceln "Total: %d (terminated: %b)" x (Workpool.is_terminated wp);
  Promise.await_exn results
  ;;
+[0] Sleeping 75: 0 -> 75
+[1] Sleeping 150: 0 -> 150
+[2] Sleeping 150: 0 -> 150
+[1] Sleeping 150: 0 -> 150
+[2] Sleeping 150: 0 -> 150
+[0] mock time is now 75
+[0] Terminating: false (terminated: false)
+[0] Terminating: true (terminated: false)
+[1] mock time is now 150
+[0] after_terminate is_error: true
+[0] Terminating: true (terminated: true)
+[0] Duration (valid): 75
+[0] Total: 10 (terminated: true)
Exception: Failure "Workpool.terminate called".
```
