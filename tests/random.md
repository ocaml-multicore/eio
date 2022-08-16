# Setting up the environment

```ocaml
# #require "eio_main";;
```

```ocaml
open Eio.Std
```

# Basic check for randomness

```ocaml
# Eio_main.run @@ fun env ->
  let src = Eio.Stdenv.secure_random env in
  let b1 = Cstruct.create 8 in
  let b2 = Cstruct.create 8 in
  Eio.Flow.read_exact src b1;
  Eio.Flow.read_exact src b2;
  assert (not (Cstruct.equal b1 b2));;
- : unit = ()
```
