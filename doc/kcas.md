Eio provides the support [kcas][] requires to implement blocking in the
lock-free software transactional memory (STM) implementation that it provides.
This means that one can use all the composable lock-free data structures and
primitives for communication and synchronization implemented using **kcas** to
communicate and synchronize between Eio fibers, raw domains, and any other
schedulers that provide the domain local await mechanism.

To demonstrate **kcas**

```ocaml
# #require "kcas"
# open Kcas
```

let's first create a couple of shared memory locations

```ocaml
# let x = Loc.make 0
val x : int Loc.t = <abstr>
# let y = Loc.make 0
val y : int Loc.t = <abstr>
```

and spawn a domain

```ocaml
# let foreign_domain = Domain.spawn @@ fun () ->
    let x = Loc.get_as (fun x -> Retry.unless (x <> 0); x) x in
    Loc.set y 22;
    x
val foreign_domain : int Domain.t = <abstr>
```

that first waits for one of the locations to change value and then writes to the
other location.

Then we run a Eio program

```ocaml
# let y = Eio_main.run @@ fun _env ->
    Loc.set x 20;
    Loc.get_as (fun y -> Retry.unless (y <> 0); y) y
val y : int = 22
```

that first writes to the location the other domain is waiting on and then waits
for the other domain to write to the other location.

Joining with the other domain

```ocaml
# y + Domain.join foreign_domain
- : int = 42
```

we arrive at the answer.
