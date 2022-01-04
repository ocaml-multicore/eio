# Setting up the environment

```ocaml
# #require "eio_main";;
```

```ocaml
open Eio.Std

let run (fn : Switch.t -> unit) =
  Eio_main.run @@ fun _e ->
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
    Switch.turn_off sw (Failure "Cancel");
    traceln "Clean up"
  );;
+Running
+Clean up
Exception: Failure "Cancel".
```

`Fibre.both`, both fibres pass:

```ocaml
# run (fun _sw ->
    Fibre.both
      (fun () -> for i = 1 to 2 do traceln "i = %d" i; Fibre.yield () done)
      (fun () -> for j = 1 to 2 do traceln "j = %d" j; Fibre.yield () done)
  );;
+i = 1
+j = 1
+i = 2
+j = 2
- : unit = ()
```

`Fibre.both`, only 1st succeeds:

```ocaml
# run (fun sw ->
      Fibre.both
        (fun () -> for i = 1 to 5 do traceln "i = %d" i; Fibre.yield () done)
        (fun () -> failwith "Failed")
    );;
+i = 1
Exception: Failure "Failed".
```

`Fibre.both`, only 2nd succeeds:

```ocaml
# run (fun sw ->
      Fibre.both
        (fun () -> Fibre.yield (); failwith "Failed")
        (fun () -> for i = 1 to 5 do traceln "i = %d" i; Fibre.yield () done)
    );;
+i = 1
Exception: Failure "Failed".
```

`Fibre.both`, first fails but the other doesn't stop:

```ocaml
# run (fun sw ->
      Fibre.both (fun () -> failwith "Failed") ignore;
      traceln "Not reached"
    );;
Exception: Failure "Failed".
```

`Fibre.both`, second fails but the other doesn't stop:

```ocaml
# run (fun sw ->
      Fibre.both ignore (fun () -> failwith "Failed");
      traceln "not reached"
    );;
Exception: Failure "Failed".
```

`Fibre.both`, both fibres fail:

```ocaml
# run (fun sw ->
      Fibre.both
        (fun () -> Eio.Cancel.protect Fibre.yield; failwith "Failed 1")
        (fun () -> Eio.Cancel.protect Fibre.yield; failwith "Failed 2")
    );;
Exception: Multiple exceptions:
Failure("Failed 1")
and
Failure("Failed 2")
```

The switch is already turned off when we try to fork. The new fibre doesn't start:

```ocaml
# run (fun sw ->
      Switch.turn_off sw (Failure "Cancel");
      Fibre.fork ~sw (fun () -> traceln "Not reached");
      traceln "Main continues"
    );;
+Main continues
Exception: Failure "Cancel".
```

You can't use a switch after leaving its scope:

```ocaml
# let sw =
    let x = ref None in
    run (fun sw -> x := Some sw);
    Option.get !x;;
val sw : Switch.t = <abstr>
# Switch.check sw;;
Exception: Invalid_argument "Switch finished!".
```

Wait for either a promise or a cancellation; cancellation first:
```ocaml
# run (fun sw ->
      let p, r = Promise.create () in
      Fibre.fork ~sw (fun () ->
        Fibre.both
          (fun () -> traceln "Waiting"; Promise.await p; traceln "Resolved")
          (fun () -> failwith "Cancelled")
      );
      Fibre.yield ();
      Promise.fulfill r ();
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
      Fibre.fork ~sw (fun () -> traceln "Waiting"; Promise.await p; traceln "Resolved");
      Promise.fulfill r ();
      Fibre.yield ();
      traceln "Now cancelling...";
      Switch.turn_off sw (Failure "Cancelled")
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
      Fibre.fork ~sw (fun () -> traceln "Waiting"; ignore (Promise.await_result p); traceln "Resolved");
      Switch.turn_off sw (Failure "Cancelled");
      Promise.fulfill r ()
    );;
+Waiting
Exception: Failure "Cancelled".
```

Wait for either a promise or a switch; promise resolves first but switch off without yielding:

```ocaml
# run (fun sw ->
      let p, r = Promise.create () in
      Fibre.fork ~sw (fun () -> traceln "Waiting"; ignore (Promise.await_result p); traceln "Resolved");
      Promise.fulfill r ();
      traceln "Now cancelling...";
      Switch.turn_off sw (Failure "Cancelled")
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
      Fibre.fork_sub ~sw ~on_error (fun sw -> traceln "Child 1"; Promise.await p);
      Fibre.fork_sub ~sw ~on_error (fun sw -> traceln "Child 2"; Promise.await p);
      Switch.turn_off sw (Failure "Cancel parent")
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
      Fibre.fork_sub ~sw ~on_error (fun sw -> traceln "Child 1"; Promise.await p1);
      Fibre.fork_sub ~sw ~on_error (fun sw -> traceln "Child 2"; Promise.await p2);
      Promise.break r1 (Failure "Child error");
      Promise.fulfill r2 ();
      Fibre.yield ();
      traceln "Parent fibre is still running"
    );;
+Child 1
+Child 2
+child: Failure("Child error")
+Parent fibre is still running
- : unit = ()
```

A child can be cancelled independently of the parent:

```ocaml
# run (fun sw ->
      let p, _ = Promise.create () in
      let on_error ex = traceln "child: %s" (Printexc.to_string ex) in
      let child = ref None in
      Fibre.fork_sub ~sw ~on_error (fun sw ->
          traceln "Child 1";
          child := Some sw;
          Promise.await ~sw p
        );
      Switch.turn_off (Option.get !child) (Failure "Cancel child");
      Fibre.yield ();
      traceln "Parent fibre is still running"
    );;
+Child 1
+child: Failure("Cancel child")
+Parent fibre is still running
- : unit = ()
```

A child error handler raises:

```ocaml
# run (fun sw ->
      let p, r = Promise.create () in
      let on_error = raise in
      Fibre.fork_sub ~sw ~on_error (fun sw -> traceln "Child"; Promise.await p);
      Promise.break r (Failure "Child error escapes");
      Fibre.yield ();
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
      Fibre.fork_sub ~sw ~on_error (fun sw -> traceln "Child"; Promise.await p);
      Promise.break r (Failure "Child error is caught");
      Fibre.yield ();
      traceln "Still running"
    );;
+Child
+caught: Failure("Child error is caught")
+Still running
- : unit = ()
```

# Release handlers

```ocaml
let release label = Fibre.yield (); traceln "release %s" label
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
  Switch.turn_off sw (Failure "Parent cancelled too!");
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
      Fibre.fork ~sw (fun () ->
        traceln "Starting release 1";
        Fibre.yield ();
        traceln "Finished release 1"
      );
    );
    Switch.on_release sw (fun () ->
      Fibre.fork ~sw (fun () ->
        Switch.on_release sw (fun () -> traceln "Late release");
        traceln "Starting release 2";
        Fibre.yield ();
        traceln "Finished release 2"
      );
    );
    traceln "Main fibre done"
  );;
+Main fibre done
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
    Fibre.fork ~sw (fun () -> try Fibre.await_cancel () with _ -> failwith "cancel1 failed");
    Fibre.fork ~sw (fun () -> try Fibre.await_cancel () with _ -> failwith "cancel2 failed");
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
