# Setting up the environment

```ocaml
# #require "eio_main";;
```

```ocaml
open Eio.Std

type clock_type = [`System | `Mono]

let clock env = function
  | `System -> Eio.Stdenv.sys_clock env
  | `Mono -> Eio.Stdenv.mono_clock env

let run ?(clock_type = `System) (fn : clock:Eio.Time.clock -> unit) =
  Eio_main.run @@ fun env ->
  let clock = clock env clock_type in
  fn ~clock

let sleep ~clock =
  let t0 = Eio.Time.now clock in
  let delay = Eio.Time.of_seconds 0.01 |> Eio.Time.to_nanoseconds in
  Eio.Time.sleep clock delay;
  let t1 = Eio.Time.now clock in
  let diff = Eio.Time.sub t1 t0 |> Eio.Time.to_seconds in
  assert (diff >= 0.01)
```
# Test cases

Check sleep works:

```ocaml
# run sleep ;;
- : unit = ()

# run ~clock_type:`Mono sleep;;
- : unit = ()
```

Check sleep works with a switch:

```ocaml
let sleep_sw ~clock =
  Eio.Switch.run @@ fun _ ->
  sleep ~clock
```

```ocaml
# run sleep_sw ;;
- : unit = ()

# run ~clock_type:`Mono sleep_sw ;;
- : unit = ()
```

Cancelling sleep:

```ocaml
let cancel ~clock =
  let delay = Eio.Time.of_seconds 1200. |> Eio.Time.to_nanoseconds in
  Fiber.both
    (fun () -> Eio.Time.sleep clock delay; assert false)
    (fun () -> failwith "Simulated cancel")
```

```ocaml
# run cancel ;;
Exception: Failure "Simulated cancel".

# run ~clock_type:`Mono cancel ;;
Exception: Failure "Simulated cancel".
```

Switch is already off:

```ocaml
let switch_off ~clock =
  Switch.run @@ fun sw ->
  Switch.fail sw (Failure "Simulated failure");
  let sleep = Eio.Time.of_seconds 1200. |> Eio.Time.to_nanoseconds in
  Eio.Time.sleep clock sleep;
  assert false;;
```

```ocaml
# run switch_off ;;
Exception: Failure "Simulated failure".

# run ~clock_type:`Mono switch_off ;;
Exception: Failure "Simulated failure".
```

Scheduling a timer that's already due:

```ocaml
let timer_due ~clock =
  Switch.run @@ fun sw ->
  let sleep = Eio.Time.of_seconds (-1.) |> Eio.Time.to_nanoseconds in
  Fiber.both
    (fun () -> traceln "First fiber runs"; Eio.Time.sleep clock sleep; traceln "Sleep done")
    (fun () -> traceln "Second fiber runs")
```

```ocaml
# run timer_due ;;
+First fiber runs
+Second fiber runs
+Sleep done
- : unit = ()

# run ~clock_type:`Mono timer_due ;;
+First fiber runs
+Second fiber runs
+Sleep done
- : unit = ()
```

Check ordering works:

```ocaml
let ordering ~clock =
  Switch.run @@ fun sw ->
  let delay1 = Eio.Time.of_seconds 1200. |> Eio.Time.to_nanoseconds in
  let delay2 = Eio.Time.of_seconds 0.1 |> Eio.Time.to_nanoseconds in
  Fiber.both
    (fun () ->
      Eio.Time.sleep clock delay1;
      assert false
    )
    (fun () ->
      Eio.Time.sleep clock delay2;
      traceln "Short timer finished";
      failwith "Simulated cancel"
    );;
```

```ocaml
# run ordering ;;
+Short timer finished
Exception: Failure "Simulated cancel".

# run ~clock_type:`Mono ordering ;;
+Short timer finished
Exception: Failure "Simulated cancel".
```

Check Unix debug clock:


```ocaml
# Eio_main.run @@ fun _ ->
  let sleep = Eio.Time.of_seconds 0.001 |> Eio.Time.to_nanoseconds in
  Fiber.both
    (fun () -> traceln "First thread starts"; Eio_unix.sleep sleep; traceln "Sleep done")
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
  Fiber.first
    loop
    (fun () -> 
      let sleep = Eio.Time.of_seconds 0.1 |> Eio.Time.to_nanoseconds in
      Eio.Time.sleep clock sleep);;
- : unit = ()

# run ~clock_type:`Mono @@ fun ~clock ->
  Fiber.first
    loop
    (fun () -> 
      let sleep = Eio.Time.of_seconds 0.1 |> Eio.Time.to_nanoseconds in
      Eio.Time.sleep clock sleep);;
- : unit = ()
```
