# A lock-free queue for schedulers

```ocaml
# #require "eio.utils";;
```

```ocaml
module Q = Eio_utils.Lf_queue;;
```

## A basic run

```ocaml
# let q : int Q.t = Q.create ();;
val q : int Q.t = <abstr>
# Q.push q 1;;
- : unit = ()
# Q.push q 2;;
- : unit = ()
# Q.pop q;;
- : int option = Some 1
# Q.pop q;;
- : int option = Some 2
# Q.pop q;;
- : int option = None
# Q.pop q;;
- : int option = None
# Q.push q 3;;
- : unit = ()
# Q.pop q;;
- : int option = Some 3
```

## Closing the queue

```ocaml
# let q : int Q.t = Q.create ();;
val q : int Q.t = <abstr>
# Q.push q 1;;
- : unit = ()
# Q.close q;;
- : unit = ()
# Q.push q 2;;
Exception: Eio_utils__Lf_queue.Closed.
# Q.push_head q 3;;
- : unit = ()
# Q.pop q;;
- : int option = Some 3
# Q.pop q;;
- : int option = Some 1
# Q.pop q;;
Exception: Eio_utils__Lf_queue.Closed.
# Q.push_head q 4;;
Exception: Eio_utils__Lf_queue.Closed.
```

## Closing an empty queue

```ocaml
# let q = Q.create () in Q.close q; Q.push q 1;;
Exception: Eio_utils__Lf_queue.Closed.
```

## Empty?

```ocaml
# let q : int Q.t = Q.create ();;
val q : int Q.t = <abstr>
# Q.is_empty q;;
- : bool = true
# Q.push q 1; Q.is_empty q;;
- : bool = false
# Q.pop q;;
- : int option = Some 1
# Q.is_empty q;;
- : bool = true
# Q.close q; Q.is_empty q;;
Exception: Eio_utils__Lf_queue.Closed.
```

## Pushing to the head

```ocaml
# let q : int Q.t = Q.create ();;
val q : int Q.t = <abstr>
# Q.push_head q 3; Q.push q 4; Q.push_head q 2; Q.push q 5; Q.push_head q 1;;
- : unit = ()
# Q.pop q;;
- : int option = Some 1
# Q.pop q;;
- : int option = Some 2
# Q.pop q;;
- : int option = Some 3
# Q.pop q;;
- : int option = Some 4
# Q.pop q;;
- : int option = Some 5
# Q.pop q;;
- : int option = None
```
