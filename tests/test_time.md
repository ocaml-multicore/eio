# Setting up the environment

```ocaml
# #require "eio_main";;
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

Check sleep works with a switch:

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
