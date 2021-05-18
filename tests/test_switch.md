# Setting up the environment

```ocaml
# #require "eunix";;
```

```ocaml
open Fibreslib

let run fn =
  try
    Eunix.run @@ fun _e ->
    Switch.top fn;
    print_endline "ok"
  with Failure msg ->
    print_endline msg
```

# Test cases

A very basic example:

```ocaml
# run (fun _sw ->
      traceln "Running"
    );
Running
ok
- : unit = ()
```

Turning off a switch still allows you to perform clean-up operations:

```ocaml
# run (fun sw ->
    traceln "Running";
    Switch.turn_off sw (Failure "Cancel");
    traceln "Clean up"
  );
Running
Clean up
Cancel
- : unit = ()
```

`Fibre.both`, both fibres pass:

```ocaml
# run (fun sw ->
    Fibre.both ~sw
      (fun () -> for i = 1 to 2 do traceln "i = %d" i; Fibre.yield ~sw () done)
      (fun () -> for j = 1 to 2 do traceln "j = %d" j; Fibre.yield ~sw () done)
  );
i = 1
j = 1
i = 2
j = 2
ok
- : unit = ()
```

`Fibre.both`, only 1st succeeds:

```ocaml
# run (fun sw ->
      Fibre.both ~sw
        (fun () -> for i = 1 to 5 do traceln "i = %d" i; Fibre.yield ~sw () done)
        (fun () -> failwith "Failed")
    )
i = 1
Failed
- : unit = ()
```

`Fibre.both`, only 2nd succeeds:

```ocaml
# run (fun sw ->
      Fibre.both ~sw
        (fun () -> Fibre.yield ~sw (); failwith "Failed")
        (fun () -> for i = 1 to 5 do traceln "i = %d" i; Fibre.yield ~sw () done)
    )
i = 1
Failed
- : unit = ()
```

`Fibre.both`, first fails but the other doesn't stop:

```ocaml
# run (fun sw ->
      Fibre.both ~sw (fun () -> failwith "Failed") ignore;
      traceln "Not reached"
    )
Failed
- : unit = ()
```

`Fibre.both`, second fails but the other doesn't stop:

```ocaml
# run (fun sw ->
      Fibre.both ~sw ignore (fun () -> failwith "Failed");
      traceln "not reached"
    )
Failed
- : unit = ()
```

`Fibre.both`, both fibres fail:

```ocaml
# run (fun sw ->
      Fibre.both ~sw
        (fun () -> failwith "Failed 1")
        (fun () -> failwith "Failed 2")
    )
Failed 1
- : unit = ()
```

The switch is already turned off when we try to fork. The new fibre doesn't start:

```ocaml
# run (fun sw ->
      Switch.turn_off sw (Failure "Cancel");
      Fibre.fork_ignore ~sw (fun () -> traceln "Not reached");
      traceln "Also not reached"
    )
Cancel
- : unit = ()
```

You can't use a switch after leaving its scope:

```ocaml
# let sw =
    let x = ref None in
    run (fun sw -> x := Some sw);
    Option.get !x
ok
val sw : Switch.t = <abstr>
# Switch.check sw
Exception: Invalid_argument "Switch finished!".
```

Turning off a switch runs the cancel callbacks, unless they've been removed by then:

```ocaml
# run (fun sw ->
      let h1 = Fibre_impl.Switch.add_cancel_hook sw (fun _ -> traceln "Cancel 1") in
      let h2 = Fibre_impl.Switch.add_cancel_hook sw (fun _ -> traceln "Cancel 2") in
      let h3 = Fibre_impl.Switch.add_cancel_hook sw (fun _ -> traceln "Cancel 3") in
      Fibre_impl.Waiters.remove_waiter h2;
      Switch.turn_off sw (Failure "Cancelled");
      let h4 = Fibre_impl.Switch.add_cancel_hook sw (fun _ -> traceln "Cancel 4") in
      Fibre_impl.Waiters.remove_waiter h1;
      Fibre_impl.Waiters.remove_waiter h3;
      Fibre_impl.Waiters.remove_waiter h4
    )
Cancel 3
Cancel 1
Cancel 4
Cancelled
- : unit = ()
```

Wait for either a promise or a switch; switch cancelled first:
```ocaml
# run (fun sw ->
      let p, r = Promise.create () in
      Fibre.fork_ignore ~sw (fun () -> traceln "Waiting"; Promise.await ~sw p; traceln "Resolved");
      Switch.turn_off sw (Failure "Cancelled");
      Promise.fulfill r ()
    )
Waiting
Cancelled
- : unit = ()
```

Wait for either a promise or a switch; promise resolves first:

```ocaml
# run (fun sw ->
      let p, r = Promise.create () in
      Fibre.fork_ignore ~sw (fun () -> traceln "Waiting"; Promise.await ~sw p; traceln "Resolved");
      Promise.fulfill r ();
      Fibre.yield ();
      traceln "Now cancelling...";
      Switch.turn_off sw (Failure "Cancelled")
    );
Waiting
Resolved
Now cancelling...
Cancelled
- : unit = ()
```

Wait for either a promise or a switch; switch cancelled first. Result version.

```ocaml
# run (fun sw ->
      let p, r = Promise.create () in
      Fibre.fork_ignore ~sw (fun () -> traceln "Waiting"; ignore (Promise.await_result ~sw p); traceln "Resolved");
      Switch.turn_off sw (Failure "Cancelled");
      Promise.fulfill r ()
    );
Waiting
Cancelled
- : unit = ()
```

Wait for either a promise or a switch; promise resolves first but switch off without yielding:

```ocaml
# run (fun sw ->
      let p, r = Promise.create () in
      Fibre.fork_ignore ~sw (fun () -> traceln "Waiting"; ignore (Promise.await_result ~sw p); traceln "Resolved");
      Promise.fulfill r ();
      traceln "Now cancelling...";
      Switch.turn_off sw (Failure "Cancelled")
    )
Waiting
Now cancelling...
Cancelled
- : unit = ()
```

Child switches are cancelled when the parent is cancelled:

```ocaml
# run (fun sw ->
      let p, _ = Promise.create () in
      let on_error ex = traceln "child: %s" (Printexc.to_string ex) in
      Fibre.fork_sub_ignore ~sw ~on_error (fun sw -> traceln "Child 1"; Promise.await ~sw p);
      Fibre.fork_sub_ignore ~sw ~on_error (fun sw -> traceln "Child 2"; Promise.await ~sw p);
      Switch.turn_off sw (Failure "Cancel parent")
    )
Child 1
Child 2
child: Failure("Cancel parent")
child: Failure("Cancel parent")
Cancel parent
- : unit = ()
```

A child can fail independently of the parent:

```ocaml
# run (fun sw ->
      let p1, r1 = Promise.create () in
      let p2, r2 = Promise.create () in
      let on_error ex = traceln "child: %s" (Printexc.to_string ex) in
      Fibre.fork_sub_ignore ~sw ~on_error (fun sw -> traceln "Child 1"; Promise.await ~sw p1);
      Fibre.fork_sub_ignore ~sw ~on_error (fun sw -> traceln "Child 2"; Promise.await ~sw p2);
      Promise.break r1 (Failure "Child error");
      Promise.fulfill r2 ();
      Fibre.yield ~sw ();
      traceln "Parent fibre is still running"
    )
Child 1
Child 2
child: Failure("Child error")
Parent fibre is still running
ok
- : unit = ()
```

A child can be cancelled independently of the parent:

```ocaml
# run (fun sw ->
      let p, _ = Promise.create () in
      let on_error ex = traceln "child: %s" (Printexc.to_string ex) in
      let child = ref None in
      Fibre.fork_sub_ignore ~sw ~on_error (fun sw ->
          traceln "Child 1";
          child := Some sw;
          Promise.await ~sw p
        );
      Switch.turn_off (Option.get !child) (Failure "Cancel child");
      Fibre.yield ~sw ();
      traceln "Parent fibre is still running"
    );
Child 1
child: Failure("Cancel child")
Parent fibre is still running
ok
- : unit = ()
```

A child error handle raises:

```ocaml
# run (fun sw ->
      let p, r = Promise.create () in
      let on_error = raise in
      Fibre.fork_sub_ignore ~sw ~on_error (fun sw -> traceln "Child"; Promise.await ~sw p);
      Promise.break r (Failure "Child error escapes");
      Fibre.yield ~sw ();
      traceln "Not reached"
    )
Child
Child error escapes
- : unit = ()
```

A child error handler deals with the exception:

```ocaml
# run (fun sw ->
      let print ex = traceln "%s" (Printexc.to_string ex); 0 in
      let x = Switch.sub ~sw ~on_error:print (fun _sw -> failwith "Child error") in
      traceln "x = %d" x
    )
Failure("Child error")
x = 0
ok
- : unit = ()
```

The system deadlocks. The scheduler detects and reports this:

```ocaml
# run (fun sw ->
      let p, _ = Promise.create () in
      Promise.await ~sw p
    )
Deadlock detected: no events scheduled but main function hasn't returned
- : unit = ()
```
