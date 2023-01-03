```ocaml
# #require "eio";;
```
```ocaml
module T = Eio__core__Broadcast
let show t = Fmt.pr "%a@." T.dump t
let fiber name () = Fmt.pr "%s: woken@." name
```

Initially we have a single segment full of empty cells.
Both resume and suspend pointers point at the first cell:

```ocaml
# let t : T.t = T.create ();;
val t : T.t = <abstr>
# show t;;
Segment 0 (prev=None, pointers=2, cancelled=0):
  Empty (suspend) (resume)
  Empty
  Empty
  Empty
End
- : unit = ()
```

## Waking an empty queue

Broadcasting with no waiters does nothing:

```ocaml
# T.resume_all t; show t;;
Segment 0 (prev=None, pointers=2, cancelled=0):
  Empty (suspend) (resume)
  Empty
  Empty
  Empty
End
- : unit = ()
```

## Adding waiters

Requesting a wake-up adds requests to the queue:
```ocaml
# T.suspend t (fiber "0");;
- : T.request option = Some <abstr>

# show t;;
Segment 0 (prev=None, pointers=2, cancelled=0):
  Request (resume)
  Empty (suspend)
  Empty
  Empty
End
- : unit = ()
```
The returned request is to allow us to cancel if desired.

Filling the segment:
```ocaml
# let suspend name = T.suspend t (fiber name);;
val suspend : string -> T.request option = <fun>

# for i = 1 to 3 do suspend (string_of_int i) |> Option.get |> ignore done;;
- : unit = ()

# show t;;
Segment 0 (prev=None, pointers=2, cancelled=0):
  Request (resume)
  Request
  Request
  Request
End (suspend)
- : unit = ()
```

Allocating new segments:
```ocaml
# let reqs = List.init 5 (fun i -> suspend (string_of_int i) |> Option.get);;
val reqs : T.request list = [<abstr>; <abstr>; <abstr>; <abstr>; <abstr>]

# show t;;
Segment 0 (prev=None, pointers=1, cancelled=0):
  Request (resume)
  Request
  Request
  Request
Segment 1 (prev=Some 0, pointers=0, cancelled=0):
  Request
  Request
  Request
  Request
Segment 2 (prev=Some 1, pointers=1, cancelled=0):
  Request
  Empty (suspend)
  Empty
  Empty
End
- : unit = ()
```

Cancelling all the cells in a segment removes the segment:
```ocaml
# List.iter (fun r -> assert (T.cancel r)) reqs; show t;;
Segment 0 (prev=None, pointers=1, cancelled=0):
  Request (resume)
  Request
  Request
  Request
End
Segment 2 (prev=Some 0, pointers=1, cancelled=1):
  Cancelled
  Empty (suspend)
  Empty
  Empty
End
- : unit = ()
```

```ocaml
# suspend "last";;
- : T.request option = Some <abstr>

# T.resume_all t;;
0: woken
1: woken
2: woken
3: woken
last: woken
- : unit = ()

# show t;;
Segment 2 (prev=None, pointers=2, cancelled=1):
  Cancelled
  Resumed
  Empty (suspend) (resume)
  Empty
End
- : unit = ()
```

Resume all, filling segment:

```ocaml
# suspend "a";;
- : T.request option = Some <abstr>
# suspend "b";;
- : T.request option = Some <abstr>

# T.resume_all t;;
a: woken
b: woken
- : unit = ()

# show t;;
Segment 2 (prev=None, pointers=2, cancelled=1):
  Cancelled
  Resumed
  Resumed
  Resumed
End (suspend) (resume)
- : unit = ()
```
