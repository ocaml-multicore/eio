# Setting up the environment

```ocaml
# #require "eio_main";;
# #require "eio.mock";;
```

```ocaml
open Eio.Std

let run (fn : clock:Eio.Time.clock -> unit) =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  fn ~clock
```

# Test cases

Check sleep works:

```ocaml
# run @@ fun ~clock ->
  let t0 = Unix.gettimeofday () in
  Eio.Time.sleep clock 0.01;
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 >= 0.01);;
- : unit = ()
```

Cancelling sleep:

```ocaml
# run @@ fun ~clock ->
  Fiber.both
    (fun () -> Eio.Time.sleep clock 1200.; assert false)
    (fun () -> failwith "Simulated cancel");;
Exception: Failure "Simulated cancel".
```

Switch is already off:

```ocaml
# run @@ fun ~clock ->
  Switch.run @@ fun sw ->
  Switch.fail sw (Failure "Simulated failure");
  Eio.Time.sleep clock 1200.0;
  assert false;;
Exception: Failure "Simulated failure".
```

Scheduling a timer that's already due:

```ocaml
# run @@ fun ~clock ->
  Switch.run @@ fun sw ->
  Fiber.both
    (fun () -> traceln "First fiber runs"; Eio.Time.sleep clock (-1.0); traceln "Sleep done")
    (fun () -> traceln "Second fiber runs");;
+First fiber runs
+Second fiber runs
+Sleep done
- : unit = ()
```

Check ordering works:

```ocaml
# run @@ fun ~clock ->
  Switch.run @@ fun sw ->
  Fiber.both
    (fun () ->
      Eio.Time.sleep clock 1200.0;
      assert false
    )
    (fun () ->
      Eio.Time.sleep clock 0.1;
      traceln "Short timer finished";
      failwith "Simulated cancel"
    );;
+Short timer finished
Exception: Failure "Simulated cancel".
```

Check Unix debug clock:
```ocaml
# Eio_main.run @@ fun _ ->
  Fiber.both
    (fun () -> traceln "First thread starts"; Eio_unix.sleep 0.001; traceln "Sleep done")
    (fun () -> traceln "Second thread starts");;
+First thread starts
+Second thread starts
+Sleep done
- : unit = ()
```

Timer and busy loop:
```ocaml
let rec loop () =
  Eio.Fiber.yield ();
  loop ()
```

```ocaml
# run @@ fun ~clock ->
  Fiber.yield ();
  Eio.Time.sleep clock 0.01;
  Fiber.first
    loop
    (fun () -> Eio.Time.sleep clock 0.01);;
- : unit = ()
```

### Timeouts

```ocaml
# run @@ fun ~clock ->
  Eio.Time.Timeout.(run_exn none) (fun () -> ());
  let t = Eio.Time.Timeout.of_s clock 0.0001 in
  Eio.Time.Timeout.run_exn t (fun () -> Fiber.await_cancel ());;
Exception: Eio__Time.Timeout.
```

```ocaml
# run @@ fun ~clock ->
  let show d =
    let t = Eio.Time.Timeout.of_s clock d in
    traceln "%g -> %a" d Eio.Time.Timeout.pp t
  in
  show 0.000000001;
  show 0.01;
  show 0.1;
  show 2.;
  show 60.;
  show 61.5;
  show 120.;
  show (30. *. 60.);
  ;;
+1e-09 -> 1e-09s
+0.01 -> 10ms
+0.1 -> 0.1s
+2 -> 2s
+60 -> 60s
+61.5 -> 62s
+120 -> 2m
+1800 -> 30m
- : unit = ()
```

### Mock clock

```ocaml
let mock = Eio_mock.Clock.make ()
let sleeper label time () = Eio.Time.sleep_until mock time; traceln "%s (%g) woken" label time
```

Advancing the time:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Fiber.all [
    sleeper "A" 5.0;
    sleeper "B" 7.0;
    sleeper "C" 2.0;
    sleeper "D" 0.0;
    sleeper "E" 5.0;
    (fun () ->
       while true do
         Fiber.yield ();
         Eio_mock.Clock.advance mock
       done
    )
  ];;
+D (0) woken
+mock time is now 2
+C (2) woken
+mock time is now 5
+A (5) woken
+E (5) woken
+mock time is now 7
+B (7) woken
Exception: Invalid_argument "No further events scheduled on mock clock".
```

Setting the time directly:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Eio_mock.Clock.set_time mock 0.0;
  Fiber.all [
    sleeper "A" 5.0;
    sleeper "B" 7.0;
    sleeper "C" 2.0;
    sleeper "D" 0.0;
    sleeper "E" 5.0;
    (fun () ->
       Fiber.yield ();
       Eio_mock.Clock.set_time mock 5.0;
       Fiber.yield ();
       Eio_mock.Clock.set_time mock 1.0;
       Fiber.yield ();
       Eio_mock.Clock.set_time mock 10.0;
       Fiber.yield ();
       Eio_mock.Clock.set_time mock 12.0
    )
  ];;
+mock time is now 0
+D (0) woken
+mock time is now 5
+C (2) woken
+A (5) woken
+E (5) woken
+mock time is now 1
+mock time is now 10
+B (7) woken
+mock time is now 12
- : unit = ()
```

Cancellation:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Eio_mock.Clock.set_time mock 0.0;
  Fiber.first
    (sleeper "A" 5.0)
    (fun () -> traceln "Cancel sleeper");
  Eio_mock.Clock.advance mock;;
+mock time is now 0
+Cancel sleeper
Exception: Invalid_argument "No further events scheduled on mock clock".
```
