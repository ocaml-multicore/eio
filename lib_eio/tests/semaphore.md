```ocaml
# #require "eio";;
```
```ocaml
module T = Eio__Sem_state
let show t = Fmt.pr "%a@." T.dump t

let acquire t label =
  if T.acquire t then (
    Fmt.pr "%s: Acquired@." label;
    None
  ) else (
    T.suspend t (fun () -> Fmt.pr "%s: Acquired@." label)
  )
```

Initially we have a single segment full of empty cells.
Both resume and suspend pointers point at the first cell:

```ocaml
# let t : T.t = T.create 1;;
val t : T.t = {T.state = <abstr>; cells = <abstr>}
# show t;;
Semaphore (state=1)
Segment 0 (prev=None, pointers=2, cancelled=0):
  In_transition (suspend) (resume)
  In_transition
  In_transition
  In_transition
End
- : unit = ()
```

The first user can take the lock just by decrementing the state counter:

```ocaml
# acquire t "a";;
a: Acquired
- : T.request option = None

# show t;;
Semaphore (state=0)
Segment 0 (prev=None, pointers=2, cancelled=0):
  In_transition (suspend) (resume)
  In_transition
  In_transition
  In_transition
End
- : unit = ()
```

However, a second user must wait:

```ocaml
# acquire t "b";;;
- : T.request option =
Some ({T.state = <abstr>; cells = <abstr>}, <abstr>, <abstr>)

# show t;;
Semaphore (state=-1)
Segment 0 (prev=None, pointers=2, cancelled=0):
  Request (resume)
  In_transition (suspend)
  In_transition
  In_transition
End
- : unit = ()
```

Same for a third user:

```ocaml
# acquire t "c";;;
- : T.request option =
Some ({T.state = <abstr>; cells = <abstr>}, <abstr>, <abstr>)

# show t;;
Semaphore (state=-2)
Segment 0 (prev=None, pointers=2, cancelled=0):
  Request (resume)
  Request
  In_transition (suspend)
  In_transition
End
- : unit = ()
```

When the first user releases it, the second one runs:

```ocaml
# T.release t;;
b: Acquired
- : unit = ()

# show t;;
Semaphore (state=-1)
Segment 0 (prev=None, pointers=2, cancelled=0):
  Finished
  Request (resume)
  In_transition (suspend)
  In_transition
End
- : unit = ()
```

When that finishes, the third one runs:

```ocaml
# T.release t;;
c: Acquired
- : unit = ()
```

The final release, with no waiters, just increments the state counter:

```ocaml
# T.release t;;
- : unit = ()

# show t;;
Semaphore (state=1)
Segment 0 (prev=None, pointers=2, cancelled=0):
  Finished
  Finished
  In_transition (suspend) (resume)
  In_transition
End
- : unit = ()
```

## Cancellation

"b" and "c" have to wait, as "a" has the resource:

```ocaml
# let t : T.t = T.create 1;;
val t : T.t = {T.state = <abstr>; cells = <abstr>}

# acquire t "a";;
a: Acquired
- : T.request option = None

# let b = acquire t "b" |> Option.get;;
val b : T.request = ({T.state = <abstr>; cells = <abstr>}, <abstr>, <abstr>)

# let c = acquire t "c" |> Option.get;;
val c : T.request = ({T.state = <abstr>; cells = <abstr>}, <abstr>, <abstr>)

# show t;;
Semaphore (state=-2)
Segment 0 (prev=None, pointers=2, cancelled=0):
  Request (resume)
  Request
  In_transition (suspend)
  In_transition
End
- : unit = ()
```

Cancelling "b" increments the state counter and its request simply becomes Finished:

```ocaml
# T.cancel b;;
- : bool = true

# show t;;
Semaphore (state=-1)
Segment 0 (prev=None, pointers=2, cancelled=1):
  Finished (resume)
  Request
  In_transition (suspend)
  In_transition
End
- : unit = ()
```

When "a" releases it, "c" is resumed:

```ocaml
# T.release t;;
c: Acquired
- : unit = ()

# show t;;
Semaphore (state=0)
Segment 0 (prev=None, pointers=2, cancelled=1):
  Finished
  Finished
  In_transition (suspend) (resume)
  In_transition
End
- : unit = ()
```

Finally, finishing "c" restores the state to 1:

```ocaml
# T.release t;;
- : unit = ()

# show t;;
Semaphore (state=1)
Segment 0 (prev=None, pointers=2, cancelled=1):
  Finished
  Finished
  In_transition (suspend) (resume)
  In_transition
End
- : unit = ()
```
