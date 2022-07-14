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
  Eio.Time.(sleep clock (of_seconds 0.01)) ;
  let t1 = Eio.Time.now clock in
  assert (t1 -. t0 >= 0.01) 
```
# Test cases

Check sleep works:

```ocaml
# run sleep ;; 
- : unit = ()

# run ~clock_type:`Mono sleep;;
- : unit = ()
```

