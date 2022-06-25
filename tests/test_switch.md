# Setting up the environment

```ocaml
# #require "eio.mock";;
```

```ocaml
open Eio.Std

let run (fn : Switch.t -> _) =
  Eio_mock.Backend.run @@ fun () ->
  Switch.run fn
```

# Test cases

A very basic example:

```ocaml
# run (fun _sw ->
      traceln "Running"
    );;
+Running
- : unit = ()
```

Turning off a switch still allows you to perform clean-up operations:

```ocaml
# run (fun sw ->
    traceln "Running";
    Switch.fail sw (Failure "Cancel");
    traceln "Clean up"
  );;
+Running
+Clean up
Exception: Failure "Cancel".
```

`Fiber.both`, both fibers pass:

```ocaml
# run (fun _sw ->
    Fiber.both
      (fun () -> for i = 1 to 2 do traceln "i = %d" i; Fiber.yield () done)
      (fun () -> for j = 1 to 2 do traceln "j = %d" j; Fiber.yield () done)
  );;
+i = 1
+j = 1
+i = 2
+j = 2
- : unit = ()
```

`Fiber.both`, only 1st succeeds:

```ocaml
# run (fun sw ->
      Fiber.both
        (fun () -> for i = 1 to 5 do traceln "i = %d" i; Fiber.yield () done)
        (fun () -> failwith "Failed")
    );;
+i = 1
Exception: Failure "Failed".
```

`Fiber.both`, only 2nd succeeds:

```ocaml
# run (fun sw ->
      Fiber.both
        (fun () -> Fiber.yield (); failwith "Failed")
        (fun () -> for i = 1 to 5 do traceln "i = %d" i; Fiber.yield () done)
    );;
+i = 1
Exception: Failure "Failed".
```

`Fiber.both`, first fails immediately and the other doesn't start:

```ocaml
# run (fun sw ->
      Fiber.both (fun () -> failwith "Failed") (fun () -> traceln "Second OK");
      traceln "Not reached"
    );;
Exception: Failure "Failed".
```

`Fiber.both`, second fails but the other doesn't stop:

```ocaml
# run (fun sw ->
      Fiber.both ignore (fun () -> failwith "Failed");
      traceln "not reached"
    );;
Exception: Failure "Failed".
```

`Fiber.both`, both fibers fail:

```ocaml
# run (fun sw ->
      Fiber.both
        (fun () -> Eio.Cancel.protect Fiber.yield; failwith "Failed 1")
        (fun () -> Eio.Cancel.protect Fiber.yield; failwith "Failed 2")
    );;
Exception: Multiple exceptions:
Failure("Failed 1")
and
Failure("Failed 2")
```

The switch is already turned off when we try to fork. The new fiber doesn't start:

```ocaml
# run (fun sw ->
      Switch.fail sw (Failure "Cancel");
      Fiber.fork ~sw (fun () -> traceln "Not reached");
      traceln "Main continues"
    );;
+Main continues
Exception: Failure "Cancel".
```

You can't use a switch after leaving its scope:

```ocaml
# let sw = run Fun.id;;
val sw : Switch.t = <abstr>
# Switch.check sw;;
Exception: Invalid_argument "Switch finished!".
```

Wait for either a promise or a cancellation; cancellation first:
```ocaml
# run (fun sw ->
      let p, r = Promise.create () in
      Fiber.fork ~sw (fun () ->
        Fiber.both
          (fun () -> traceln "Waiting"; Promise.await p; traceln "Resolved")
          (fun () -> failwith "Cancelled")
      );
      Fiber.yield ();
      Promise.resolve r ();
      traceln "Main thread done";
    );;
+Waiting
+Main thread done
Exception: Failure "Cancelled".
```

Wait for either a promise or a switch; promise resolves first:

```ocaml
# run (fun sw ->
      let p, r = Promise.create () in
      Fiber.fork ~sw (fun () -> traceln "Waiting"; Promise.await p; traceln "Resolved");
      Promise.resolve r ();
      Fiber.yield ();
      traceln "Now cancelling...";
      Switch.fail sw (Failure "Cancelled")
    );;
+Waiting
+Resolved
+Now cancelling...
Exception: Failure "Cancelled".
```

Wait for either a promise or a switch; switch cancelled first. Result version.

```ocaml
# run (fun sw ->
      let p, r = Promise.create () in
      Fiber.fork ~sw (fun () -> traceln "Waiting"; Promise.await p; traceln "Resolved");
      Switch.fail sw (Failure "Cancelled");
      Promise.resolve r ()
    );;
+Waiting
Exception: Failure "Cancelled".
```

Wait for either a promise or a switch; promise resolves first but switch off without yielding:

```ocaml
# run (fun sw ->
      let p, r = Promise.create () in
      Fiber.fork ~sw (fun () -> traceln "Waiting"; Promise.await p; traceln "Resolved");
      Promise.resolve r ();
      traceln "Now cancelling...";
      Switch.fail sw (Failure "Cancelled")
    );;
+Waiting
+Now cancelling...
+Resolved
Exception: Failure "Cancelled".
```

Child switches are cancelled when the parent is cancelled, but `on_error` isn't notified:

```ocaml
# run (fun sw ->
      let p, _ = Promise.create () in
      let on_error ex = traceln "child: %s" (Printexc.to_string ex) in
      Fiber.fork_sub ~sw ~on_error (fun sw -> traceln "Child 1"; Promise.await p);
      Fiber.fork_sub ~sw ~on_error (fun sw -> traceln "Child 2"; Promise.await p);
      Switch.fail sw (Failure "Cancel parent")
    );;
+Child 1
+Child 2
Exception: Failure "Cancel parent".
```

A child can fail independently of the parent:

```ocaml
# run (fun sw ->
      let p1, r1 = Promise.create () in
      let p2, r2 = Promise.create () in
      let on_error ex = traceln "child: %s" (Printexc.to_string ex) in
      Fiber.fork_sub ~sw ~on_error (fun sw -> traceln "Child 1"; Promise.await_exn p1);
      Fiber.fork_sub ~sw ~on_error (fun sw -> traceln "Child 2"; Promise.await_exn p2);
      Promise.resolve_error r1 (Failure "Child error");
      Promise.resolve_ok r2 ();
      Fiber.yield ();
      traceln "Parent fiber is still running"
    );;
+Child 1
+Child 2
+child: Failure("Child error")
+Parent fiber is still running
- : unit = ()
```

A child can be cancelled independently of the parent:

```ocaml
# run (fun sw ->
      let p, _ = Promise.create () in
      let on_error ex = traceln "child: %s" (Printexc.to_string ex) in
      let child = ref None in
      Fiber.fork_sub ~sw ~on_error (fun sw ->
          traceln "Child 1";
          child := Some sw;
          Promise.await ~sw p
        );
      Switch.fail (Option.get !child) (Failure "Cancel child");
      Fiber.yield ();
      traceln "Parent fiber is still running"
    );;
+Child 1
+child: Failure("Cancel child")
+Parent fiber is still running
- : unit = ()
```

A child error handler raises:

```ocaml
# run (fun sw ->
      let p, r = Promise.create () in
      let on_error = raise in
      Fiber.fork_sub ~sw ~on_error (fun sw -> traceln "Child"; Promise.await_exn p);
      Promise.resolve_error r (Failure "Child error escapes");
      Fiber.yield ();
      traceln "Not reached"
    );;
+Child
Exception: Failure "Child error escapes".
```

A child error handler deals with the exception:

```ocaml
# run (fun sw ->
      let p, r = Promise.create () in
      let on_error = traceln "caught: %a" Fmt.exn in
      Fiber.fork_sub ~sw ~on_error (fun sw -> traceln "Child"; Promise.await_exn p);
      Promise.resolve_error r (Failure "Child error is caught");
      Fiber.yield ();
      traceln "Still running"
    );;
+Child
+caught: Failure("Child error is caught")
+Still running
- : unit = ()
```

# Release handlers

```ocaml
let release label = Fiber.yield (); traceln "release %s" label
```

Release on success:

```ocaml
# run (fun sw ->
    Switch.on_release sw (fun () -> release "1");
    Switch.on_release sw (fun () -> release "2");
  );;
+release 2
+release 1
- : unit = ()
```

Release on error:

```ocaml
# run (fun sw ->
    Switch.on_release sw (fun () -> release "1");
    Switch.on_release sw (fun () -> release "2");
    failwith "Test error"
  );;
+release 2
+release 1
Exception: Failure "Test error".
```

A release operation itself fails:

```ocaml
# run (fun sw ->
    Switch.on_release sw (fun () -> release "1"; failwith "failure 1");
    Switch.on_release sw (fun () -> release "2");
    Switch.on_release sw (fun () -> release "3"; failwith "failure 3");
  );;
+release 3
+release 2
+release 1
Exception: Multiple exceptions:
Failure("failure 3")
and
Failure("failure 1")
```

Attaching a release handler to a finished switch from a cancelled context:

```ocaml
# run @@ fun sw ->
  let sub = Switch.run Fun.id in        (* A finished switch *)
  Switch.fail sw (Failure "Parent cancelled too!");
  Switch.on_release sub (fun () -> release "1");;
+release 1
Exception:
Multiple exceptions:
Failure("Parent cancelled too!")
and
Invalid_argument("Switch finished!")
```

Using switch from inside release handler:

```ocaml
# run (fun sw ->
    Switch.on_release sw (fun () ->
      Fiber.fork ~sw (fun () ->
        traceln "Starting release 1";
        Fiber.yield ();
        traceln "Finished release 1"
      );
    );
    Switch.on_release sw (fun () ->
      Fiber.fork ~sw (fun () ->
        Switch.on_release sw (fun () -> traceln "Late release");
        traceln "Starting release 2";
        Fiber.yield ();
        traceln "Finished release 2"
      );
    );
    traceln "Main fiber done"
  );;
+Main fiber done
+Starting release 2
+Starting release 1
+Finished release 2
+Finished release 1
+Late release
- : unit = ()
```

# Error reporting

All release hooks run, even if some fail, and all errors are reported:

```ocaml
# run (fun sw ->
    Fiber.fork ~sw (fun () -> try Fiber.await_cancel () with _ -> failwith "cancel1 failed");
    Fiber.fork ~sw (fun () -> try Fiber.await_cancel () with _ -> failwith "cancel2 failed");
    raise Exit
  );;
Exception:
Multiple exceptions:
Stdlib.Exit
and
Failure("cancel2 failed")
and
Failure("cancel1 failed")
```

# Errors during cleanup are reported during cancellation

```ocaml
# run (fun sw ->
    Fiber.fork ~sw (fun () ->
      Switch.run @@ fun sw ->
      try Fiber.await_cancel () with _ -> failwith "cleanup failed");
    Fiber.fork ~sw (fun () -> failwith "simulated error")
  );;
Exception:
Multiple exceptions:
Failure("simulated error")
and
Failure("cleanup failed")
```
