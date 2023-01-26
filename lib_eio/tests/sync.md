```ocaml
# #require "eio";;
# #mod_use "dscheck/fake_sched.ml";;
module Fake_sched :
  sig
    val cancel : Eio.Cancel.t -> unit
    val run : (unit -> unit) -> Eio.Cancel.t option
  end
```
```ocaml
module T = Eio__Sync
module Fiber_context = Eio.Private.Fiber_context

let show t = Fmt.pr "%a@." T.dump t

let put t v =
  Fake_sched.run
    (fun () ->
      match T.put t v with
      | () -> Fmt.pr "Sent %s@." v
      | exception (Eio.Cancel.Cancelled _) -> Fmt.pr "Send of %s was cancelled@." v
    )
  |> Option.map (fun ctx ->
    Fmt.pr "Waiting for a consumer for %s@." v;
    ctx
  )

let take t label =
  Fake_sched.run
    (fun () ->
      match T.take t with
      | v -> Fmt.pr "%s: Took %s@." label v
      | exception (Eio.Cancel.Cancelled _) -> Fmt.pr "%s: Take cancelled@." label
    )
  |> Option.map (fun ctx ->
    Fmt.pr "%s: Waiting for producer@." label;
    ctx
  )
```

Initially there are no consumers or producers:

```ocaml
# let t : string T.t = T.create ();;
val t : string T.t = <abstr>
# show t;;
Sync (balance=0)
  Consumers:
    Segment 0 (prev=None, pointers=2, cancelled=0):
      In_transition (suspend) (resume)
      In_transition
      In_transition
      In_transition
    End
  Producers:
    Segment 0 (prev=None, pointers=2, cancelled=0):
      In_transition (suspend) (resume)
      In_transition
      In_transition
      In_transition
    End
- : unit = ()
```

Adding one consumer makes the balance go negative:

```ocaml
# take t "cons1";;
cons1: Waiting for producer
- : Eio.Cancel.t option = Some <abstr>

# show t;;
Sync (balance=-1)
  Consumers:
    Segment 0 (prev=None, pointers=2, cancelled=0):
      Slot (resume)
      In_transition (suspend)
      In_transition
      In_transition
    End
  Producers:
    Segment 0 (prev=None, pointers=2, cancelled=0):
      In_transition (suspend) (resume)
      In_transition
      In_transition
      In_transition
    End
- : unit = ()
```

Sending a value wakes it:

```ocaml
# put t "A";;
cons1: Took A
Sent A
- : Eio.Cancel.t option = None

# show t;;
Sync (balance=0)
  Consumers:
    Segment 0 (prev=None, pointers=2, cancelled=0):
      Finished
      In_transition (suspend) (resume)
      In_transition
      In_transition
    End
  Producers:
    Segment 0 (prev=None, pointers=2, cancelled=0):
      In_transition (suspend) (resume)
      In_transition
      In_transition
      In_transition
    End
- : unit = ()
```

Trying to send a second value waits on the producers queue, setting the balance to 1:

```ocaml
# put t "B";;
Waiting for a consumer for B
- : Eio.Cancel.t option = Some <abstr>

# show t;;
Sync (balance=1)
  Consumers:
    Segment 0 (prev=None, pointers=2, cancelled=0):
      Finished
      In_transition (suspend) (resume)
      In_transition
      In_transition
    End
  Producers:
    Segment 0 (prev=None, pointers=2, cancelled=0):
      Item (resume)
      In_transition (suspend)
      In_transition
      In_transition
    End
- : unit = ()
```

Sending a third value must also wait:
```ocaml
# put t "C";;
Waiting for a consumer for C
- : Eio.Cancel.t option = Some <abstr>

# show t;;
Sync (balance=2)
  Consumers:
    Segment 0 (prev=None, pointers=2, cancelled=0):
      Finished
      In_transition (suspend) (resume)
      In_transition
      In_transition
    End
  Producers:
    Segment 0 (prev=None, pointers=2, cancelled=0):
      Item (resume)
      Item
      In_transition (suspend)
      In_transition
    End
- : unit = ()
```

The next consumer reads the first value and wakes the first producer:
```ocaml
# take t "cons2";;
Sent B
cons2: Took B
- : Eio.Cancel.t option = None

# show t;;
Sync (balance=1)
  Consumers:
    Segment 0 (prev=None, pointers=2, cancelled=0):
      Finished
      In_transition (suspend) (resume)
      In_transition
      In_transition
    End
  Producers:
    Segment 0 (prev=None, pointers=2, cancelled=0):
      Finished
      Item (resume)
      In_transition (suspend)
      In_transition
    End
- : unit = ()
```

Finally, we collect the last value:
```ocaml
# take t "cons3";;
Sent C
cons3: Took C
- : Eio.Cancel.t option = None
```

## Cancellation

Cancelling a consumer restores the balance:
```ocaml
# let t : string T.t = T.create ();;
val t : string T.t = <abstr>
# let request = take t "cons1" |> Option.get;;
cons1: Waiting for producer
val request : Eio.Cancel.t = <abstr>

# show t;;
Sync (balance=-1)
  Consumers:
    Segment 0 (prev=None, pointers=2, cancelled=0):
      Slot (resume)
      In_transition (suspend)
      In_transition
      In_transition
    End
  Producers:
    Segment 0 (prev=None, pointers=2, cancelled=0):
      In_transition (suspend) (resume)
      In_transition
      In_transition
      In_transition
    End
- : unit = ()
```

```ocaml
# Fake_sched.cancel request;;
cons1: Take cancelled
- : unit = ()

# show t;;
Sync (balance=0)
  Consumers:
    Segment 0 (prev=None, pointers=2, cancelled=1):
      Finished (resume)
      In_transition (suspend)
      In_transition
      In_transition
    End
  Producers:
    Segment 0 (prev=None, pointers=2, cancelled=0):
      In_transition (suspend) (resume)
      In_transition
      In_transition
      In_transition
    End
- : unit = ()
```

Cancelling a producer restores the balance count:

```ocaml
# let t : string T.t = T.create ();;
val t : string T.t = <abstr>
# let a = put t "A" |> Option.get;;
Waiting for a consumer for A
val a : Eio.Cancel.t = <abstr>
# put t "B" |> Option.get;;
Waiting for a consumer for B
- : Eio.Cancel.t = <abstr>

# show t;;
Sync (balance=2)
  Consumers:
    Segment 0 (prev=None, pointers=2, cancelled=0):
      In_transition (suspend) (resume)
      In_transition
      In_transition
      In_transition
    End
  Producers:
    Segment 0 (prev=None, pointers=2, cancelled=0):
      Item (resume)
      Item
      In_transition (suspend)
      In_transition
    End
- : unit = ()

# Fake_sched.cancel a;;
Send of A was cancelled
- : unit = ()

# show t;;
Sync (balance=1)
  Consumers:
    Segment 0 (prev=None, pointers=2, cancelled=0):
      In_transition (suspend) (resume)
      In_transition
      In_transition
      In_transition
    End
  Producers:
    Segment 0 (prev=None, pointers=2, cancelled=1):
      Finished (resume)
      Item
      In_transition (suspend)
      In_transition
    End
- : unit = ()
```

The next consumer sees the second value:

```ocaml
# take t "cons4";;
Sent B
cons4: Took B
- : Eio.Cancel.t option = None

# show t;;
Sync (balance=0)
  Consumers:
    Segment 0 (prev=None, pointers=2, cancelled=0):
      In_transition (suspend) (resume)
      In_transition
      In_transition
      In_transition
    End
  Producers:
    Segment 0 (prev=None, pointers=2, cancelled=1):
      Finished
      Finished
      In_transition (suspend) (resume)
      In_transition
    End
- : unit = ()
```
